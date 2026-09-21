#!/bin/bash
# Exports sample posts for the prototype feed from redesign_import, the
# production dump in the new schema, into data/<handle>.js:
#   ./redesign/prototype/export-sample.sh valorant valheim
# The development stack's postgres container has to be running, and
# redesign/import/import.sh has to have built redesign_import in it. data/ is
# git-ignored: the sample is real players' content and stays on this machine.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
# The dump's date: the prototype treats it as now, so freshness reads as it did.
now="2026-09-12T08:23:58Z"

mkdir -p "$here/data"
for handle in "$@"; do
    json="$(docker exec -i postgres psql -U bklaric -d redesign_import -At -v ON_ERROR_STOP=1 \
        -v handle="$handle" -v now="$now" < "$here/export-sample.sql")"
    printf 'FEED_DATA = %s;\n' "$json" > "$here/data/$handle.js"
    echo "$here/data/$handle.js"
done
