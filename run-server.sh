#!/bin/bash
fuser -k 8080/tcp
node dist-server/server.js &>> dist-server/log.txt &
