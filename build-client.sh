#!/bin/bash
set -e
cd "$(dirname "$0")"
# Tools come from devDependencies, so everyone builds with the same versions.
# Safe only because nothing below invokes spago -- see the note in build.sh.
export PATH="$PWD/node_modules/.bin:$PATH"
# Emptied rather than removed: a running Caddy has the directory itself mounted.
mkdir -p release/client
rm -rf release/client/*
mkdir release/client/images
mkdir release/client/images/games
mkdir release/client/fonts
cp src/TeamTavern/Client/Static/index.html release/client/index.html
cp src/TeamTavern/Client/Static/index.prerender.html release/client/index.prerender.html
cp src/TeamTavern/Client/Static/ads.txt release/client/ads.txt
cp src/TeamTavern/Client/Static/favicon.svg release/client/favicon.svg
cp src/TeamTavern/Client/Static/logo-mark.svg release/client/logo-mark.svg
cp src/TeamTavern/Client/Static/logo-512.png release/client/logo-512.png
cp -r src/TeamTavern/Client/Static/Images/Games/. release/client/images/games/
cp src/TeamTavern/Client/Static/Fonts/* release/client/fonts/
discriminator=`openssl rand -hex 8`
sass src/TeamTavern/Client/Style/Main.scss "release/client/style.min.${discriminator}.css" --style compressed
echo "import { main } from './output/TeamTavern.Client.Main/index.js'; main();" | esbuild --bundle --outfile=release/client/app.min.${discriminator}.js --minify
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" release/client/index.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" release/client/index.html
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" release/client/index.prerender.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" release/client/index.prerender.html
