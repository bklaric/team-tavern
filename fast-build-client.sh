#!/bin/bash
rm -rf dist-client
mkdir dist-client
mkdir dist-client/images
mkdir dist-client/favicons
mkdir dist-client/css
mkdir dist-client/webfonts
cp src/TeamTavern/Client/Static/robots.txt dist-client/robots.txt
cp src/TeamTavern/Client/Static/index.html dist-client/index.html
cp src/TeamTavern/Client/Static/Images/* dist-client/images/
cp src/TeamTavern/Client/Static/Favicons/* dist-client/favicons/
cp src/TeamTavern/Client/Static/Css/* dist-client/css/
cp src/TeamTavern/Client/Static/Fonts/* dist-client/webfonts/
discriminator=`openssl rand -hex 8`
sassc src/TeamTavern/Client/Style/Main.scss > "dist-client/style.min.${discriminator}.css"
spago bundle-app --main TeamTavern.Client.Main --to "dist-client/app.min.${discriminator}.js" --no-install --no-build
sed -i -e "s/app.min.js/app.min.$discriminator.js/g" dist-client/index.html
sed -i -e "s/style.min.css/style.min.$discriminator.css/g" dist-client/index.html
