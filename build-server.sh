#!/bin/bash
# spago bundle-app --main TeamTavern.Server.Main --to dist-server/server.js --no-install --no-build --platform node
esbuild output/TeamTavern.Server.Main/index.js --outfile=dist-server/server.js --platform=node --bundle --format=cjs --external:bcrypt --external:pg --external:@sendgrid/mail
echo "main();" >> dist-server/server.js
