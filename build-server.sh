#!/bin/bash
set -e
cd "$(dirname "$0")"
# Tools come from devDependencies, so everyone builds with the same versions.
# Safe only because nothing below invokes spago -- see the note in build.sh.
export PATH="$PWD/node_modules/.bin:$PATH"
mkdir -p release/server
cp package.json release/server/package.json
cp package-lock.json release/server/package-lock.json
esbuild output/TeamTavern.Server.Main/index.js --outfile=release/server/server.js --platform=node --bundle --format=cjs --external:bcrypt --external:pg --external:@sendgrid/mail --loader:.sql=text
echo "main();" >> release/server/server.js
# The relaunch's one email, run by hand in the node container (redesign/relaunch.md).
esbuild output/TeamTavern.RelaunchEmail.Main/index.js --outfile=release/server/relaunch-email.js --platform=node --bundle --format=cjs --external:bcrypt --external:pg --external:@sendgrid/mail --loader:.sql=text
echo "main();" >> release/server/relaunch-email.js
# The test stack's Discord, kept out of release/ so it never reaches production.
mkdir -p dist-test
esbuild output/TeamTavern.DiscordStub.Main/index.js --outfile=dist-test/discord-stub.js --platform=node --bundle --format=cjs
echo "main();" >> dist-test/discord-stub.js
# And its SendGrid.
esbuild output/TeamTavern.MailStub.Main/index.js --outfile=dist-test/mail-stub.js --platform=node --bundle --format=cjs
echo "main();" >> dist-test/mail-stub.js
