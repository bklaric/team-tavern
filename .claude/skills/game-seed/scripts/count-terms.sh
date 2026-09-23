#!/usr/bin/env bash
# Counts the posts in the production dump whose text mentions each term: player
# and team profiles of one game, about and ambitions together, lower-cased.
#
#   count-terms.sh <old-handle> <label>=<regex>...
#   count-terms.sh valorant 'duelist=\mduelists?\M' 'smokes=\msmok(e|es|er)\M' 'igl=\migl\M'
#
# The regex is a Postgres one (\m and \M are word boundaries) matched against
# the lower-cased text. The handle is the dump's, which may predate a rename
# (csgo, not counter-strike-2). Run from the repo root with the development stack up.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "usage: $0 <old-handle> <label>=<regex>..." >&2
    exit 2
fi

handle="$1"
shift

user=$(grep '^POSTGRES_USER=' stacks/.env | cut -d= -f2 | tr -d '\r')

terms=""
for term in "$@"; do
    label="${term%%=*}"
    regex="${term#*=}"
    terms+="${terms:+, }(\$\$${label}\$\$, \$\$${regex}\$\$)"
done

docker exec -i postgres psql -U "$user" -d team_tavern -v ON_ERROR_STOP=1 -q <<SQL
with post as (
    select lower(array_to_string(about || ambitions, ' ')) as text, updated
    from (
        select game_id, about, ambitions, updated from player_profile
        union all
        select game_id, about, ambitions, updated from team_profile
    ) profile
    join game on game.id = profile.game_id
    where game.handle = \$\$${handle}\$\$
),
dump as (
    select max(updated) as taken from post
)
select term.label as term,
    count(*) filter (where post.text ~ term.regex) as posts,
    count(*) filter (where post.text ~ term.regex and post.updated > dump.taken - interval '2 years') as last_two_years,
    count(*) as of_posts
from post
cross join dump
cross join (values ${terms}) as term (label, regex)
group by term.label
order by posts desc;
SQL
