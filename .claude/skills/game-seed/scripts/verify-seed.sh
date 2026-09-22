#!/usr/bin/env bash
# Applies the schema and game seeds to a throwaway database in the development
# stack's postgres container, prints each game's fields, runs the catalogue
# invariants, checks every game has its cover, and drops the database again.
#
#   verify-seed.sh <seed.sql>...   the given games alone
#   verify-seed.sh --all           every game in the seed directory, together
#
# Run from the repo root. Each run gets its own database, so several can run at
# once. Exits non-zero when a seed fails to apply or an invariant fails; a
# missing cover is reported but doesn't fail the run, since the cover is an
# asset that may land separately.

set -euo pipefail

database=src/TeamTavern/Database
schema=$database/TablesCurrent.sql
games_dir=$database/Seed/Games
# Countries reference regions, so the order matters.
regions=("$database/Seed/Regions.sql" "$database/Seed/Countries.sql")
covers_dir=src/TeamTavern/Client/Static/Images/Games

if [ $# -eq 0 ]; then
    echo "usage: $0 <seed.sql>... | --all" >&2
    exit 2
fi
if [ "$1" = "--all" ]; then
    seeds=("$games_dir"/*.sql)
else
    seeds=("$@")
fi

user=$(grep '^POSTGRES_USER=' stacks/.env | cut -d= -f2 | tr -d '\r')
db="seed_check_$$_$RANDOM"

psql_admin() { docker exec -i postgres psql -U "$user" -d postgres -q "$@"; }
psql_db() { docker exec -i postgres psql -U "$user" -d "$db" -v ON_ERROR_STOP=1 -q "$@"; }

psql_admin -c "create database $db;"
trap 'psql_admin -c "drop database if exists $db;" >/dev/null' EXIT

psql_db < "$schema"
for f in "${regions[@]}"; do psql_db < "$f"; done
for f in "${seeds[@]}"; do
    if ! psql_db < "$f"; then
        echo "FAILED to apply: $f" >&2
        exit 1
    fi
done
echo "Applied ${#seeds[@]} seed(s) onto $schema."

psql_db -c "
select g.handle, f.ordinal as n, f.key, f.ilk, f.ordered, f.slotted,
       f.applies_to, f.on_card, count(o.id) as options
from game g
join field f on f.game_id = g.id
left join field_option o on o.field_id = f.id
group by g.handle, f.id
order by g.handle, f.ordinal;"

psql_db -c "
select g.handle, string_agg(c.kind, ', ' order by c.kind) as contacts,
       (select string_agg(t.title || ' (' || t.contact_kind || ')', ', ')
        from tracker t where t.game_id = g.id) as trackers
from game g
join game_contact c on c.game_id = g.id
group by g.id, g.handle
order by g.handle;"

# The fields every game answers in its own words, side by side, for the pass
# over the whole catalogue that follows a parallel run.
psql_db -c "
select g.handle, f.key,
       coalesce(string_agg(o.label, ', ' order by o.ordinal), '(' || f.ilk || ')') as options
from game g
join field f on f.game_id = g.id
left join field_option o on o.field_id = f.id
where f.key = 'looking-for' or f.slotted
group by g.handle, f.id, f.key
order by f.key, g.handle;"

# The intents every game shares, in order, and the words that give one away
# when it turns up under another key.
shared="(values
    ('casual', 'Casual', 1),
    ('ranked', 'Ranked', 2),
    ('scrims-tournaments', 'Scrims and tournaments', 3),
    ('learning-the-game', 'Learning the game', 4)
) as shared (key, label, ordinal)"
shared_words='casual|rank|competitive|tournament|scrim|league|learn|new|beginner|returning'

problems=$(psql_db -tA -c "
select 'shared Looking for option under another label: ' || g.handle || '.' || o.key
from game g join field f on f.game_id = g.id join field_option o on o.field_id = f.id
join $shared on shared.key = o.key
where f.key = 'looking-for' and o.label <> shared.label
union all
select 'Looking for option that reads as a shared intent: ' || g.handle || '.' || o.key
from game g join field f on f.game_id = g.id join field_option o on o.field_id = f.id
where f.key = 'looking-for'
  and not exists (select 1 from $shared where shared.key = o.key)
  and (o.key ~* '$shared_words' or o.label ~* '$shared_words')
union all
select 'Looking for options not shared first, in the shared order: ' || g.handle
from game g join field f on f.game_id = g.id join field_option o on o.field_id = f.id
left join $shared on shared.key = o.key
where f.key = 'looking-for'
group by g.handle
having array_agg(o.key order by o.ordinal)
    <> array_agg(o.key order by shared.ordinal nulls last, o.ordinal)
union all
select 'role option that is a job or an any-slot, not a slot: ' || g.handle || '.' || f.key || '.' || o.key
from game g join field f on f.game_id = g.id join field_option o on o.field_id = f.id
where f.slotted and o.key ~ '^(in-game-leader|igl|flex|fill|any)$'
union all
select 'no looking-for field: ' || g.handle
from game g
where not exists (select 1 from field f where f.game_id = g.id and f.key = 'looking-for')
union all
select 'looking-for must be multi, on the card, on all three types: ' || g.handle
from game g join field f on f.game_id = g.id
where f.key = 'looking-for'
  and (f.ilk <> 'multi' or not f.on_card or f.applies_to <> '{player,group,community}')
union all
select 'platform must be multi, on the card, on all three types: ' || g.handle
from game g join field f on f.game_id = g.id
where f.key = 'platform'
  and (f.ilk <> 'multi' or not f.on_card or f.applies_to <> '{player,group,community}')
union all
select 'platform field with fewer than two options: ' || g.handle
from game g join field f on f.game_id = g.id
where f.key = 'platform' and (select count(*) from field_option o where o.field_id = f.id) < 2
union all
select 'no discord contact: ' || g.handle
from game g
where not exists (select 1 from game_contact c where c.game_id = g.id and c.kind = 'discord')
union all
select 'ordered field is not single: ' || g.handle || '.' || f.key
from game g join field f on f.game_id = g.id
where f.ordered and f.ilk <> 'single'
union all
select 'slotted field is not multi: ' || g.handle || '.' || f.key
from game g join field f on f.game_id = g.id
where f.slotted and f.ilk <> 'multi'
union all
select 'boolean field with options: ' || g.handle || '.' || f.key
from game g join field f on f.game_id = g.id
where f.ilk = 'boolean' and exists (select 1 from field_option o where o.field_id = f.id)
union all
select 'ordered or slotted field reaches communities: ' || g.handle || '.' || f.key
from game g join field f on f.game_id = g.id
where (f.ordered or f.slotted) and 'community' = any(f.applies_to)
union all
select 'ordered field with fewer than two options: ' || g.handle || '.' || f.key
from game g join field f on f.game_id = g.id
where f.ordered and (select count(*) from field_option o where o.field_id = f.id) < 2
union all
select 'no field leads the card: ' || g.handle
from game g
where not exists (select 1 from field f where f.game_id = g.id and f.on_card)
union all
select 'field ordinals are not 1..n: ' || g.handle
from game g join field f on f.game_id = g.id
group by g.id, g.handle
having count(distinct f.ordinal) <> count(*) or min(f.ordinal) <> 1 or max(f.ordinal) <> count(*)
union all
select 'option ordinals are not 1..n: ' || g.handle || '.' || f.key
from game g join field f on f.game_id = g.id join field_option o on o.field_id = f.id
group by g.handle, f.id, f.key
having count(distinct o.ordinal) <> count(*) or min(o.ordinal) <> 1 or max(o.ordinal) <> count(*)
union all
select 'option key is not kebab-case: ' || g.handle || '.' || f.key || '.' || o.key
from game g join field f on f.game_id = g.id join field_option o on o.field_id = f.id
where o.key !~ '^[a-z0-9]+(-[a-z0-9]+)*$';")

if [ -n "$problems" ]; then
    echo "Invariants failed:" >&2
    echo "$problems" | sed 's/^/  /' >&2
    exit 1
fi
echo "Invariants hold."

missing=0
for handle in $(psql_db -tA -c "select handle from game order by handle;"); do
    if [ ! -f "$covers_dir/$handle.webp" ]; then
        echo "No cover: $covers_dir/$handle.webp"
        missing=1
    fi
done
[ $missing -eq 0 ] && echo "Every game has its cover."
exit 0
