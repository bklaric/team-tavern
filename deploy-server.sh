#!/bin/bash
pulp build --main ServerMain --to dist-server/server.js

fuser -k 8080/tcp

node dist-server/server.js > dist-server/log.txt &
