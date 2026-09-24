#!/bin/bash
set -e
cd "$(dirname "$0")"
# Tools come from devDependencies, so everyone builds with the same versions.
# Safe only because nothing below invokes spago -- see the note in build.sh.
export PATH="$PWD/node_modules/.bin:$PATH"
mkdir -p dist-client
rm -rf dist-client/*
mkdir dist-client/images
mkdir dist-client/images/games
mkdir dist-client/fonts
cp src/TeamTavern/Client/Static/index.html dist-client/index.html
cp src/TeamTavern/Client/Static/index.prerender.html dist-client/index.prerender.html
cp src/TeamTavern/Client/Static/ads.txt dist-client/ads.txt
cp src/TeamTavern/Client/Static/favicon.svg dist-client/favicon.svg
cp src/TeamTavern/Client/Static/logo-mark.svg dist-client/logo-mark.svg
cp src/TeamTavern/Client/Static/logo-512.png dist-client/logo-512.png
cp -r src/TeamTavern/Client/Static/Images/Games/. dist-client/images/games/
cp src/TeamTavern/Client/Static/Fonts/* dist-client/fonts/
discriminator=`openssl rand -hex 8`
sass src/TeamTavern/Client/Style/Main.scss "dist-client/style.min.${discriminator}.css" --style compressed
echo "import { main } from './output/TeamTavern.Client.Main/index.js'; main();" | esbuild --bundle --outfile=dist-client/app.min.${discriminator}.js --minify
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" dist-client/index.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" dist-client/index.html
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" dist-client/index.prerender.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" dist-client/index.prerender.html
