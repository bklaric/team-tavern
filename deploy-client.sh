#!/bin/bash
rm -rf dist-client
mkdir dist-client
cp robots.txt dist-client/robots.txt
cp index.html dist-client/index.html
cp src/TeamTavern/Client/Favicons/* dist-client/
sass src/TeamTavern/Client/Style/Main.scss \
    | cleancss -o dist-client/style.min.css
pulp build --main TeamTavern.Client.Main --optimise \
    | uglifyjs --beautify beautify=false,ascii_only=true --mangle --compress \
    > dist-client/app.min.js
