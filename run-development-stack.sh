#!/bin/bash
cd "$(dirname "$0")"
# The release as a server runs it, configured by the development stack's .env.
docker compose --env-file stacks/.env -f release/compose.yml up -d --force-recreate --remove-orphans
