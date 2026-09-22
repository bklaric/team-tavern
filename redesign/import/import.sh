#!/usr/bin/env bash
# Builds a database in the new schema from the production dump in the
# development stack's postgres container, and prints what the import dropped
# (brief 12).
#
#   redesign/import/import.sh [database]    default: redesign_import
#
# Run from the repo root. The dump's database, team_tavern, is only read:
# pg_dump copies it into the target, where it becomes the legacy schema beside
# the new one. The target is dropped and rebuilt on every run.

set -euo pipefail

target="${1:-redesign_import}"
user=$(grep '^POSTGRES_USER=' stacks/.env | cut -d= -f2 | tr -d '\r')

psql_admin() { docker exec -i postgres psql -U "$user" -d postgres -v ON_ERROR_STOP=1 -q "$@"; }
psql_target() { docker exec -i postgres psql -U "$user" -d "$target" -v ON_ERROR_STOP=1 -q "$@"; }

psql_admin -c "drop database if exists $target;" -c "create database $target;"

docker exec postgres pg_dump -U "$user" --no-owner --no-privileges team_tavern | psql_target >/dev/null
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
