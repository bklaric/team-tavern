#!/bin/bash
pulp build --main ServerMain --to dist/server.js

node dist/server.js > dist/log.txt &
