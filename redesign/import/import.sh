#!/usr/bin/env bash
# Builds a database in the new schema from the old site's database in the
# tt-postgres container, and prints what the import dropped (brief 12).
#
#   redesign/import/import.sh [database]    default: redesign_import
#
# Run from the repo root, or from wherever redesign/import/ and
# src/TeamTavern/Database/ were unpacked beside each other on a server
# (redesign/relaunch.md). POSTGRES_USER and POSTGRES_DB name the user and the
# old site's database; where they are not set, they come from stacks/.env.
#
# The old database is only read: pg_dump copies it into the target, where it
# becomes the legacy schema beside the new one. The target is dropped and
# rebuilt on every run.

set -euo pipefail

target="${1:-redesign_import}"
from_env() { grep "^$1=" stacks/.env | cut -d= -f2 | tr -d '\r'; }
user="${POSTGRES_USER:-$(from_env POSTGRES_USER)}"
source="${POSTGRES_DB:-$(from_env POSTGRES_DB)}"

if [ "$target" = "$source" ]; then
    echo "The target, $target, is the database it imports from." >&2
    exit 1
fi

psql_admin() { docker exec -i tt-postgres psql -U "$user" -d postgres -v ON_ERROR_STOP=1 -q "$@"; }
psql_target() { docker exec -i tt-postgres psql -U "$user" -d "$target" -v ON_ERROR_STOP=1 -q "$@"; }

psql_admin -c "drop database if exists $target;" -c "create database $target;"

docker exec tt-postgres pg_dump -U "$user" --no-owner --no-privileges "$source" | psql_target >/dev/null
psql_target -c "alter schema public rename to legacy;" -c "create schema public;"

database=src/TeamTavern/Database
cat "$database/TablesCurrent.sql" "$database/Seed/Regions.sql" "$database/Seed/Countries.sql" "$database"/Seed/Games/*.sql \
    | psql_target

{ echo 'begin;'; cat redesign/import/mapping.sql redesign/import/import.sql; echo 'commit;'; } \
    | psql_target

# A restore leaves the planner without statistics, and the feed query's plans
# go wrong without them.
psql_target -c "vacuum analyze;"

psql_target < redesign/import/report.sql
