#!/usr/bin/env bash
# Times feed.sql against redesign_import, the production dump in the new schema
# (redesign/import/import.sh), with now at the dump's date:
#
#   redesign/feed/bench.sh [database] [runs]    defaults: redesign_import 20
#
# Each case is prepared once and executed runs times, as the server would send
# it: node-pg's unnamed statements are planned for their parameters every
# time, so the session forces custom plans. Prints each case's best and median
# execution time: on a loaded machine the best is the steadiest figure.

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
database="${1:-redesign_import}"
runs="${2:-20}"
user=$(grep '^POSTGRES_USER=' "$here/../../stacks/.env" | cut -d= -f2 | tr -d '\r')
now="2026-09-12T08:23:58Z"

# name|handle|viewer|description|types
cases=(
    "empty, all types|valorant|null|{\"type\": \"player\"}|{player,group,community}"
    "player, every field|valorant|null|{\"type\": \"player\", \"options\": {\"rank\": [\"diamond-2\"], \"role\": [\"controller\", \"sentinel\"], \"platform\": [\"pc\"], \"looking-for\": [\"ranked\"]}, \"flags\": [\"in-game-leader\"], \"country\": \"Croatia\", \"age\": 24, \"languages\": [\"English\"], \"online\": {\"from\": \"19:00\", \"to\": \"23:00\"}, \"timezone\": \"Europe/Zagreb\", \"microphone\": true}|{player,group,community}"
    "player, rank and role, players only|valorant|null|{\"type\": \"player\", \"options\": {\"rank\": [\"gold-1\"], \"role\": [\"duelist\"]}}|{player}"
    "group, every field|valorant|null|{\"type\": \"group\", \"options\": {\"role\": [\"controller\", \"initiator\"], \"platform\": [\"pc\"]}, \"ranges\": {\"rank\": {\"from\": \"platinum-1\", \"to\": \"diamond-3\"}}, \"regions\": [\"Europe\"], \"ageFrom\": 18, \"ageTo\": 30, \"languages\": [\"English\", \"German\"], \"online\": {\"from\": \"20:00\", \"to\": \"02:00\"}, \"timezone\": \"Europe/Berlin\", \"microphone\": true}|{player}"
    "lol player, every field|lol|null|{\"type\": \"player\", \"options\": {\"role\": [\"mid\"]}, \"country\": \"Germany\", \"languages\": [\"English\"], \"online\": {\"from\": \"18:00\", \"to\": \"22:00\"}, \"timezone\": \"Europe/Berlin\", \"microphone\": true}|{player,group,community}"
)

psql_db() { docker exec -i postgres psql -U "$user" -d "$database" -v ON_ERROR_STOP=1 -qAt "$@"; }

query="$(cat "$here/feed.sql")"

run_case() {
    local handle="$1" viewer="$2" description="$3" types="$4" cursor="$5" analyze="$6"
    {
        echo "set plan_cache_mode = force_custom_plan;"
        echo "prepare feed (text, integer, jsonb, text[], jsonb, timestamptz) as"
        echo "$query;"
        for _ in $(seq "$runs"); do
            echo "explain (analyze, timing off, summary on, format json) execute feed('$handle', $viewer, '$description', '$types', $cursor, '$now');"
        done
        if [ "$analyze" = yes ]; then
            echo "explain (analyze, buffers) execute feed('$handle', $viewer, '$description', '$types', $cursor, '$now');"
        fi
    } | psql_db
}

summarize() {
    grep -o '"Execution Time": [0-9.]*\|"Planning Time": [0-9.]*' | awk -v runs="$runs" '
        /Planning/ { plan[++p] = $3 }
        /Execution/ { exec[++e] = $3 }
        END {
            n = asort(exec); asort(plan)
            printf "execution best %7.1f ms, median %7.1f ms; planning best %5.1f ms\n",
                exec[1], exec[int((n + 1) / 2)], plan[1]
        }'
}

for c in "${cases[@]}"; do
    IFS='|' read -r name handle viewer description types <<< "$c"
    printf '%-38s ' "$name"
    run_case "$handle" "$viewer" "$description" "$types" null no | summarize

    # The second batch, from the first batch's last cursor.
    cursor="$(
        {
            echo "prepare feed (text, integer, jsonb, text[], jsonb, timestamptz) as"
            echo "$query;"
            echo "execute feed('$handle', $viewer, '$description', '$types', null, '$now');"
        } | psql_db -F $'\t' | awk -F'\t' 'END { print $NF }'
    )"
    printf '%-38s ' "  second batch"
    run_case "$handle" "$viewer" "$description" "$types" "'$cursor'" no | summarize
done

