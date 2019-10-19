#!/bin/bash
spago bundle-app --main TeamTavern.Server.Main --to dist-server/server.js --purs-args "--output deploy-output"

fuser -k 8080/tcp

node dist-server/server.js > dist-server/log.txt &
