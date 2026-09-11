#!/bin/bash
set -e
cd "$(dirname "$0")"

# Compile PureScript, then bundle the client and the server.
#
# Spago is invoked by path and NOT through `npx`, and node_modules/.bin must not
# be on PATH here. npx puts node_modules/.bin first, which makes spago resolve
# `purs` to the `purs.cmd` shim; since Node 20.12 `child_process.spawn` refuses
# to execute .cmd/.bat without `shell: true`, so the build dies with
# `Error: spawn EINVAL`. Called this way, spago resolves the real
# node_modules/purescript/purs.bin (a normal .exe) instead.
./node_modules/.bin/spago build

# These run as separate processes, so the PATH they set does not leak back here.
./build-client.sh
./build-server.sh
