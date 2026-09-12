#!/bin/bash

# Builds the test database on the first boot of the postgres volume.
#
# TablesCurrent.sql rather than TablesBase.sql plus the migrations: the
# migrations repair production rows by id, and two of them fail outright against
# an empty database.

# test.env points PGHOST at the postgres service for the node container, but the
# server this script talks to is the temporary one the entrypoint runs, which
# listens on the local socket only.
unset PGHOST

apply() {
    psql --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" \
        --set ON_ERROR_STOP=1 --quiet --file "$1" || exit 1
}

apply /database/TablesCurrent.sql

apply /database/Seed/Regions.sql

for game in /database/Seed/Games/*.sql; do
    apply "$game"
done

apply /test-seed/players.sql
