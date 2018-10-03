#!/bin/bash
pulp build --main ServerMain --to dist/server.js

fuser -k 8080/tcp

node dist/server.js > dist/log.txt &
