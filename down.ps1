# Take the local stack down. Postgres keeps its data in ../postgres-docker, so the site's database
# and the cluster's others are still there on the next .\up.ps1.
docker compose -f ./dist/local/docker-compose.yml down --remove-orphans
