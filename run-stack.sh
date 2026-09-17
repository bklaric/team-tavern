#!/bin/bash
docker compose -f stacks/docker-compose.yml up -d --force-recreate --remove-orphans
