#!/bin/bash
set -e
cd "$(dirname "$0")"

# Uploads release/ into ~/team-tavern on a server and brings its stack up there:
#
#     ./build.sh && ./deploy-release.sh user@host
#
# That directory keeps the server's own .env beside compose.yml. A release
# carries none, and one found in release/ is left out, so an upload never
# replaces it.

host=${1:?usage: ./deploy-release.sh <user@host>}
remote=team-tavern

if [ ! -f release/compose.yml ] || [ ! -f release/server/server.js ] || [ ! -f release/client/index.html ]; then
    echo "release/ is incomplete. Run ./build.sh first." >&2
    exit 1
fi

# Compose refuses to start without a variable the release requires, `${NAME:?}`,
# and by then the files it runs are already replaced. So the server's .env is
# checked for each before anything is uploaded.
required=$(grep -o '\${[A-Z_]*:?' release/compose.yml | tr -d '${:?' | sort -u)
missing=$(ssh "$host" "cd $remote && for name in $(echo $required); do grep -q \"^\$name=.\" .env || echo \$name; done")
if [ -n "$missing" ]; then
    echo "The server's .env doesn't set:" $missing >&2
    exit 1
fi

# The index files go up last, so no page is served that names a script or
# stylesheet still on its way. Hashed names never collide, so the ones they
# replace stay where they are for pages already open. -mkdir goes on past a
# directory that is already there.
{
    find release -type d | sort | sed "s|^release\(.*\)|-mkdir \"$remote\1\"|"
    find release -type f ! -name .env ! -path 'release/client/index*.html' \
        | sed "s|^release/\(.*\)|put \"release/\1\" \"$remote/\1\"|"
    find release -type f -path 'release/client/index*.html' \
        | sed "s|^release/\(.*\)|put \"release/\1\" \"$remote/\1\"|"
} | sftp -b - "$host"

# The files are bind-mounted, so the containers are recreated to pick them up.
ssh "$host" "cd $remote && docker compose up -d --force-recreate --remove-orphans"
