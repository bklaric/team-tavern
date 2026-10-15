#!/bin/bash

# Builds the site's database, PGDATABASE in the local stack's .env, from the
# schema and the seed files the compose file mounts. A database that is
# already there is left as it is, unless --fresh, which builds it anew. The
# cluster's other databases are never touched.
#
# Runs inside tt-postgres: up.ps1 runs it, and test-playwright/stack.setup.ts
# runs it with --fresh.

set -euo pipefail

site="$PGDATABASE"
# Built under another name and renamed once complete, so a seed that fails
# leaves the site's database as it was rather than half built.
building="${site}_building"

# Over TCP: the server the entrypoint starts to initialize an empty data
# directory listens on the socket only, so this waits for the one that stays.
export PGHOST=127.0.0.1
# Only what went wrong: no notice that a database to drop isn't there.
export PGOPTIONS="-c client_min_messages=warning"

for _ in $(seq 120); do
    pg_isready --quiet --dbname postgres && break
    sleep 0.5
done
pg_isready --quiet --dbname postgres || { echo "Postgres did not start." >&2; exit 1; }

admin() {
    psql --dbname postgres --set ON_ERROR_STOP=1 --quiet --no-align --tuples-only "$@"
}

if [ "${1:-}" != "--fresh" ] && [ -n "$(admin --command "select 1 from pg_database where datname = '$site'")" ]; then
    exit 0
fi

admin --command "drop database if exists $building with (force)"
admin --command "create database $building"

# The seed's selects print the rows they made, which nothing reads.
apply() {
    psql --dbname "$building" --set ON_ERROR_STOP=1 --quiet --output /dev/null --file "$1"
}

apply /database/TablesCurrent.sql

apply /database/Seed/Regions.sql
apply /database/Seed/Countries.sql

for game in /database/Seed/Games/*.sql; do
    apply "$game"
done

apply /test-seed/players.sql

# Force ends the node container's connections, which its pool opens afresh
# on the rebuilt database.
admin --command "drop database if exists $site with (force)"
admin --command "alter database $building rename to $site"
