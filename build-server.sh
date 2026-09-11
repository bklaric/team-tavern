#!/bin/bash
set -e
cd "$(dirname "$0")"
# Tools come from devDependencies, so everyone builds with the same versions.
# Safe only because nothing below invokes spago -- see the note in build.sh.
export PATH="$PWD/node_modules/.bin:$PATH"
mkdir -p dist-server
cp package.json dist-server/package.json
cp package-lock.json dist-server/package-lock.json
esbuild output/TeamTavern.Server.Main/index.js --outfile=dist-server/server.js --platform=node --bundle --format=cjs --external:bcrypt --external:pg --external:@sendgrid/mail
echo "main();" >> dist-server/server.js
