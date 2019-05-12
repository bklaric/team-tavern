#!/bin/bash
pulp build --main TeamTavern.Server.Main --to dist-server/server.js

fuser -k 8080/tcp

node dist-server/server.js > dist-server/log.txt &
