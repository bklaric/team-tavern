#!/bin/bash
rm -rf dist-client
mkdir dist-client
mkdir dist-client/static
cp robots.txt dist-client/robots.txt
cp index.html dist-client/index.html
cp src/TeamTavern/Client/Favicons/* dist-client/
cp src/TeamTavern/Client/Static/* dist-client/static/
sass src/TeamTavern/Client/Style/Main.scss \
    | cleancss -o dist-client/style.min.css
spago bundle-app --main TeamTavern.Client.Main --to dist-client/app.js --no-install --no-build
uglifyjs --beautify beautify=false,ascii_only=true --mangle --compress -- dist-client/app.js \
    > dist-client/app.min.js
