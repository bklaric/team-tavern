#!/bin/bash
pulp build --main ServerMain --to dist/server.js

node server.js > dist/log.txt &
