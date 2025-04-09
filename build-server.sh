#!/bin/bash
cp package.json dist-server/package.json
cp package-lock.json dist-server/package-lock.json
esbuild output/TeamTavern.Server.Main/index.js --outfile=dist-server/server.js --platform=node --bundle --format=cjs --external:bcrypt --external:pg --external:@sendgrid/mail
echo "main();" >> dist-server/server.js
