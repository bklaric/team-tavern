# Relaunch runbook

What takes production from the old site to the relaunch: a release in place of
the git checkout, Postgres 18 in place of 14, and the database rebuilt by the
import in the new schema. Step 19 of `plan.md` rehearsed it on the development
stack against a production dump, and its notes hold the numbers to compare
with.

Commands marked **local** run in the repo, the rest on the server. `$host` is
the `user@host` the site is deployed to.

## How production runs before it

`~/team-tavern` is a checkout of `master`. Its stack is
`stacks/docker-compose.yml`, run from `~/team-tavern` as the compose project
`stacks`, with the server's own `stacks/.env`: containers `postgres` (14.4,
data in `$POSTGRES_DOCKER_PATH` at `/var/lib/postgresql/data`), `node`,
`renderready` and `caddy`. The nightly backup reads that `.env`.

The release runs as the project `teamtavern` from `~/team-tavern/compose.yml`
with `~/team-tavern/.env`, and its containers are `tt-postgres`, `tt-node`,
`tt-renderready` and `tt-caddy`. Postgres 18 can't open a 14 data directory, so
the database goes across as a dump, into a data directory of its own. The old
one is never written to, and rolling back is starting the old stack again.

## 1. Before the day

Check the server is as described, and that its compose is new enough for
`dockerfile_inline` (2.17 or later):

```bash
docker ps --format '{{.Names}} {{.Image}}'
docker compose version
cd ~/team-tavern && git status --short && git log --oneline -1
cat stacks/.env
```

On the Discord developer portal, the app's OAuth2 redirects include
`https://www.teamtavern.net/signin`, the one the relaunch uses on production,
and `https://staging.teamtavern.net/signin` if staging runs.

**Local:** build the release from the commit being shipped, and send the
import and the release's compose file to `~/relaunch` on the server:

```bash
git status --short     # clean
./build.sh
tar czf - redesign/import src/TeamTavern/Database/TablesCurrent.sql src/TeamTavern/Database/Seed \
    | ssh "$host" 'rm -rf ~/relaunch && mkdir ~/relaunch && tar xzf - -C ~/relaunch'
scp release/compose.yml "$host":relaunch/compose.yml
```

On the server, write `~/relaunch/.env`, the release's `.env`, from the values
in `~/team-tavern/stacks/.env`:

```bash
ENVIRONMENT=production

POSTGRES_USER=<as before>
POSTGRES_PASSWORD=<as before>
POSTGRES_DB=<as before>

# A new, empty directory. Postgres 18 keeps its data in 18/docker below it.
POSTGRES_DOCKER_PATH=<the old POSTGRES_DOCKER_PATH>-18
# As before.
POSTGRES_BACKUP_PATH=<as before>
# As before, so the certificates come along.
CADDY_DOCKER_PATH=<as before>

SENDGRID_API_KEY=<as before>

# Where reports of players are mailed.
ADMIN_EMAIL=admin@teamtavern.net
```

`TEAMTAVERN_PATH`, `DEPLOYMENT` and the `PG*` variables are gone, and
`CADDY_HTTP_PORT` stays unset so Caddy takes 80 and 443. Then create the data
directory and build and pull the images while the old site still runs, so the
downtime doesn't wait on them. The renderready build takes a while:

```bash
mkdir "<the new POSTGRES_DOCKER_PATH>"
cd ~/relaunch && docker compose build && docker compose pull --ignore-buildable
```

## 2. Stop the old site and dump its database

```bash
cd ~/team-tavern && set -a && . stacks/.env && set +a
docker stop caddy renderready node

# Should list only postgres and $POSTGRES_DB. Dump anything else the same way.
docker exec postgres psql -U "$POSTGRES_USER" -d postgres -Atc \
    "select datname from pg_database where not datistemplate"

# 18's pg_dump, on the old container's network, since a newer pg_dump reads an
# older server and not the other way round.
docker run --rm --network container:postgres -e PGPASSWORD="$POSTGRES_PASSWORD" postgres:18.6 \
    pg_dump -h localhost -U "$POSTGRES_USER" -Fc "$POSTGRES_DB" > ~/team_tavern-pg14.dump
ls -l ~/team_tavern-pg14.dump

docker compose -f stacks/docker-compose.yml down
cd ~ && mv team-tavern team-tavern-old
```

`down` keeps the old `node_modules` volume and touches no data. **Local:** keep
a copy of the dump off the server:

```bash
scp "$host":team_tavern-pg14.dump .
```

## 3. Postgres 18: restore, import, rename

```bash
mkdir ~/team-tavern && cp ~/relaunch/.env ~/relaunch/compose.yml ~/team-tavern/
cd ~/team-tavern && set -a && . ./.env && set +a
docker compose up -d tt-postgres
until docker exec tt-postgres pg_isready -h localhost -U "$POSTGRES_USER" >/dev/null; do sleep 1; done

docker exec -i tt-postgres pg_restore -U "$POSTGRES_USER" -d "$POSTGRES_DB" --exit-on-error \
    < ~/team_tavern-pg14.dump
docker exec tt-postgres vacuumdb -U "$POSTGRES_USER" -d "$POSTGRES_DB" --analyze-only
```

A restore carries no planner statistics, so `vacuumdb --analyze-only` gathers
them. The import reads `POSTGRES_USER` and `POSTGRES_DB` from the environment
set above, builds the new database beside the old one and prints its report:

```bash
cd ~/relaunch && redesign/import/import.sh team_tavern_relaunch 2>&1 | tee ~/relaunch/report.txt
```

Read the report against the rehearsal's in `plan.md`: the same kinds of drop,
and counts a day or two of sign-ups and posts on from it. A mapping error stops
the import before it writes anything. Then put the new database in the old
one's place, keeping the old one beside it:

```bash
docker exec tt-postgres psql -U "$POSTGRES_USER" -d postgres -v ON_ERROR_STOP=1 \
    -c "alter database $POSTGRES_DB rename to ${POSTGRES_DB}_legacy" \
    -c "alter database team_tavern_relaunch rename to $POSTGRES_DB"
```

## 4. Deploy

**Local:**

```bash
./deploy-release.sh "$host"
```

It uploads `release/` into `~/team-tavern`, beside the `.env`, and brings the
whole stack up on the imported database.

## 5. Check it

```bash
docker logs tt-node          # the server's start, no errors
curl -sI https://teamtavern.net/ | head -3                           # 308 to www
curl -s https://www.teamtavern.net/api/games | head -c 300
curl -s https://www.teamtavern.net/robots.txt
curl -s https://www.teamtavern.net/sitemap.xml | head -20
curl -sI https://www.teamtavern.net/games/lol/players | grep -i location   # the new feed
curl -s -A Googlebot https://www.teamtavern.net/games/valorant | grep -o '<title>[^<]*'
```

In a browser: sign in with a password account the import brought over, look at
the home page, its posts, the account page, a feed and a post, and sign out.
Then the Discord check of `CLAUDE.md` on production: Continue with Discord at
`https://www.teamtavern.net/signin` signs in a Discord account the old site
knew, without asking for a nickname, and `docker logs tt-node` shows no Discord
errors.

Replace the backup's line in the crontab with `cron.txt`'s, which reads
`~/team-tavern/.env`:

```bash
crontab -e
```

The worker's first run is an hour after node starts. `docker logs tt-node`
then shows no `Error in the period worker`. Every imported address is
unconfirmed, so that run emails only players who have confirmed since.

## 6. The relaunch email

Once the site has held for a day, one email goes to every imported player seen
in the year before the relaunch whose address is unconfirmed: what changed,
and the link that confirms the address (`RelaunchEmail/Main.purs`). Count, then
send:

```bash
docker exec tt-node node /root/team-tavern/server/relaunch-email.js
docker exec tt-node node /root/team-tavern/server/relaunch-email.js --send
```

It sends one at a time and logs each failure. A run cut short is run again:
whoever has a link is skipped. Watch SendGrid's activity for bounces and spam
reports over the next days.

Submit `https://www.teamtavern.net/sitemap.xml` in Search Console.

## Rolling back

While nothing on the new site is worth keeping:

```bash
cd ~/team-tavern && docker compose down
cd ~ && mv team-tavern team-tavern-relaunch && mv team-tavern-old team-tavern
cd ~/team-tavern && docker compose -f stacks/docker-compose.yml up -d
```

and put the crontab line back to the one reading `stacks/.env`. The old data
directory was never opened by 18, so the old site comes back as it stopped.

## Once it holds

On the server, after a week or so:

```bash
rm -rf ~/team-tavern-old ~/relaunch ~/team_tavern-pg14.dump
rm -rf "<the old POSTGRES_DOCKER_PATH>"
docker exec tt-postgres dropdb -U "$POSTGRES_USER" "${POSTGRES_DB}_legacy"
docker volume rm stacks_node_modules
docker image rm postgres:14.4 caddy:2.10.0 stacks-renderready
```

On the Discord app, remove the old site's redirect URIs, `/register` and
`/preboarding/register` on each origin, and keep the `/signin` ones. In the
repo, delete `redesign/import/`, which addresses rows only the old production
had, and `RelaunchEmail/` with its line in `build-server.sh`.
`TablesBase.sql` and `TablesCurrent.sql` are already the same file.
