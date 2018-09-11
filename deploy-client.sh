#!/bin/bash
rm -rf ~/team-tavern-nginx
mkdir ~/team-tavern-nginx
cp index.html ~/team-tavern-nginx/index.html
sass src/TeamTavern/Client/Style/Main.scss \
    | cleancss -o ~/team-tavern-nginx/style.min.css
pulp build --main ClientMain --optimise \
    | uglifyjs --compress --mangle --mangle-props \
    > ~/team-tavern-nginx/app.min.js
