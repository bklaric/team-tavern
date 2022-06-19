#!/bin/bash
rm -rf dist-client
mkdir dist-client
mkdir dist-client/images
mkdir dist-client/images/competitions
mkdir dist-client/favicons
mkdir dist-client/css
mkdir dist-client/webfonts
cp src/TeamTavern/Client/Static/robots.txt dist-client/robots.txt
cp src/TeamTavern/Client/Static/sitemap.txt dist-client/sitemap.txt
cp src/TeamTavern/Client/Static/index.html dist-client/index.html
cp src/TeamTavern/Client/Static/index.prerender.html dist-client/index.prerender.html
cp -r src/TeamTavern/Client/Static/Images/Landing/* dist-client/images/
cp -r src/TeamTavern/Client/Static/Images/Competitions/* dist-client/images/competitions/
cp src/TeamTavern/Client/Static/Favicons/* dist-client/favicons/
cp src/TeamTavern/Client/Static/Css/* dist-client/css/
cp src/TeamTavern/Client/Static/Fonts/* dist-client/webfonts/
discriminator=`openssl rand -hex 8`
sass src/TeamTavern/Client/Style/Main.scss \
    | cleancss -o "dist-client/style.min.${discriminator}.css"
spago bundle-app --main TeamTavern.Client.Main --to dist-client/app.min.${discriminator}.js --no-install --no-build
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" dist-client/index.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" dist-client/index.html
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" dist-client/index.prerender.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" dist-client/index.prerender.html
