# Relaunch runbook

What takes production from the old site to the relaunch. Step 19 of `plan.md`
writes the rest of it; the Postgres upgrade is here already because the
relaunch's compose file is what brings Postgres 18.

## Postgres 14 to 18

Production runs `postgres:14.4` with its data in `$POSTGRES_DOCKER_PATH`,
mounted at `/var/lib/postgresql/data`. The release runs `postgres:18.6`, which
cannot open a 14 data directory and keeps its own in `18/docker` below
`/var/lib/postgresql`, the path the release mounts instead. So the database
goes across as a dump: taken from 14, restored into a fresh 18.

The dump is taken with 18's `pg_dump`, run from the 18 image on the stack's
network, since a newer `pg_dump` reads an older server but not the other way
round.

### 1. On the server: stop the site and dump the database

```bash
cd ~/team-tavern && set -a && . ./.env && set +a
docker compose stop tt-caddy tt-renderready tt-node

# Should list only postgres and $POSTGRES_DB. Dump anything else the same way.
docker exec tt-postgres psql -U "$POSTGRES_USER" -d postgres -Atc \
    "select datname from pg_database where not datistemplate"

docker run --rm --network teamtavern_default -e PGPASSWORD="$POSTGRES_PASSWORD" postgres:18.6 \
    pg_dump -h tt-postgres -U "$POSTGRES_USER" -Fc "$POSTGRES_DB" > ~/team_tavern-pg14.dump
ls -l ~/team_tavern-pg14.dump

docker compose down
mv "$POSTGRES_DOCKER_PATH" "$POSTGRES_DOCKER_PATH-14" && mkdir "$POSTGRES_DOCKER_PATH"
```

`down` keeps the `node_modules` volume; only `-v` would drop it. The old data
directory is moved aside, not deleted, so rolling back is moving it back.

### 2. Locally: deploy

```bash
./build.sh && ./deploy-release.sh user@host
```

This brings the whole stack up, Postgres 18 on an empty `$POSTGRES_DB`, so the
API answers with errors until step 3 is done.

### 3. On the server: restore and bring the site back

```bash
cd ~/team-tavern && set -a && . ./.env && set +a
docker compose stop tt-caddy tt-node
until docker exec tt-postgres pg_isready -h localhost -U "$POSTGRES_USER" >/dev/null; do sleep 1; done

docker exec -i tt-postgres pg_restore -U "$POSTGRES_USER" -d "$POSTGRES_DB" --exit-on-error \
    < ~/team_tavern-pg14.dump
docker exec tt-postgres vacuumdb -U "$POSTGRES_USER" -d "$POSTGRES_DB" --analyze-only
```

A restore carries no planner statistics, so `vacuumdb --analyze-only` gathers
them before the site's queries run.

The relaunch's import runs here, against the restored database and before the
site comes back. Then:

```bash
docker compose up -d
docker logs tt-node
```

### Rolling back

```bash
docker compose down
rm -rf "$POSTGRES_DOCKER_PATH" && mv "$POSTGRES_DOCKER_PATH-14" "$POSTGRES_DOCKER_PATH"
```

then deploy a release built from before the move to Postgres 18, whose compose
file runs 14.4 on the old mount path.

### Once it holds

Delete `$POSTGRES_DOCKER_PATH-14` and `~/team_tavern-pg14.dump`. The nightly
backup runs `pg_dump` inside `tt-postgres`, so it dumps with 18 without a
change.
