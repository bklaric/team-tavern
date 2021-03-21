#!/bin/bash
fuser -k 8080/tcp
node dist-server/server.js | cat >> dist-server/log.txt &
