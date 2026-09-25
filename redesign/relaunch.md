# Relaunch runbook

What takes production from the old site to the relaunch: a new server in place
of the DigitalOcean Droplet, a release in place of the git checkout, Postgres 18
in place of 14, and the database rebuilt by the import in the new schema. Step
19 of `plan.md` rehearsed it on the development stack and ran it on production,
and its notes hold the numbers to compare with.

Commands marked **local** run in Git Bash in the repo, those marked **old** on
the Droplet, and the rest on the new server. `$old` and `$new` are their
`user@host`.

## The two servers

The old site runs on a DigitalOcean Droplet in FRA1 on Ubuntu 22.04, which DNS
reaches through its reserved IP, 67.207.77.252. `~/team-tavern` there is a
checkout of `master`. Its stack is `stacks/docker-compose.yml`, run as the
compose project `stacks` with the Droplet's own `stacks/.env`: containers
`postgres` (14.4, data in `~/postgres-docker`), `node`, `renderready` and
`caddy`. The nightly backup reads that `.env`.

The relaunch runs on a Hetzner server on Ubuntu 26.04, at 2.28.230.206, as the
project `teamtavern` from `~/team-tavern/compose.yml` with
`~/team-tavern/.env`: containers `tt-postgres`, `tt-node`, `tt-renderready` and
`tt-caddy`. Postgres 18 can't open a 14 data directory anyway, so the database
goes across as a dump. The Droplet is never written to, and rolling back is
starting its containers again and pointing DNS back at it.

## 1. Before the day

Days ahead, at Namecheap, set the TTL of the `A` records for `@` and `www` to
5 minutes. Resolvers keep an answer for its old TTL, Namecheap's Automatic
being 30 minutes, so the switch spreads within minutes only once that has run
out.

On the Discord developer portal, the app's OAuth2 redirects include
`https://www.teamtavern.net/signin`, the one the relaunch uses on production,
and `https://staging.teamtavern.net/signin` if staging runs.

The new server has its packages upgraded, Docker Engine and the Compose plugin
from Docker's apt repository, and the deploying user in the `docker` group.
Give it the stack's directories and `.env`:

```bash
mkdir ~/team-tavern ~/postgres-docker ~/postgres-backup ~/caddy-docker
nano ~/team-tavern/.env
chmod 600 ~/team-tavern/.env
```

```bash
ENVIRONMENT=production

POSTGRES_USER=bklaric
POSTGRES_PASSWORD=<new>
POSTGRES_DB=team_tavern

POSTGRES_DOCKER_PATH=/home/bklaric/postgres-docker
POSTGRES_BACKUP_PATH=/home/bklaric/postgres-backup
CADDY_DOCKER_PATH=/home/bklaric/caddy-docker

SENDGRID_API_KEY=<the real key, from the Droplet's stacks/.env>

# Where reports of players are mailed.
ADMIN_EMAIL=admin@teamtavern.net
```

The cluster is new, so its password is too: letters and digits, since the
backup's cron line reads the file through `xargs`. `CADDY_HTTP_PORT` stays unset
so Caddy takes 80 and 443. Caddy's directory starts empty, and Caddy gets its
certificates once DNS points at the server.

**Local:** build the release from the commit being shipped, and send the import
to `~/relaunch` and the compose file to `~/team-tavern`:

```bash
git status --short     # clean
./build.sh
tar czf - redesign/import src/TeamTavern/Database/TablesCurrent.sql src/TeamTavern/Database/Seed \
    | ssh "$new" 'rm -rf ~/relaunch && mkdir ~/relaunch && tar xzf - -C ~/relaunch'
scp release/compose.yml "$new":team-tavern/compose.yml
```

Then build and pull the images while the old site still runs, so the downtime
doesn't wait on them. The renderready build takes a while:

```bash
cd ~/team-tavern && docker compose build && docker compose pull --ignore-buildable
```

## 2. Stop the old site and dump its database

**Old:** pull 18's image before stopping anything. A newer `pg_dump` reads an
older server and not the other way round, so the dump is taken with 18's:

```bash
docker pull postgres:18.6

cd ~/team-tavern && set -a && . stacks/.env && set +a
docker stop caddy renderready node

# Should list only postgres and $POSTGRES_DB. Dump anything else the same way.
docker exec postgres psql -U "$POSTGRES_USER" -d postgres -Atc \
    "select datname from pg_database where not datistemplate"

docker run --rm --network container:postgres -e PGPASSWORD="$POSTGRES_PASSWORD" postgres:18.6 \
    pg_dump -h localhost -U "$POSTGRES_USER" -Fc "$POSTGRES_DB" > ~/team_tavern-pg14.dump
ls -l ~/team_tavern-pg14.dump
```

`postgres` stays up and the checkout stays where it is. Comment out the backup
line in the Droplet's crontab with `crontab -e`, or it goes on mailing the
stopped database every night.

**Local:** keep a copy of the dump off both servers, and pass it on:

```bash
mkdir -p ~/relaunch-backup && cd ~/relaunch-backup
scp "$old":team_tavern-pg14.dump .
scp team_tavern-pg14.dump "$new":
```

## 3. Point DNS at the new server

At Namecheap, change both `A` records to the new server's address, and wait for
it to be what resolvers give:

```bash
nslookup teamtavern.net 1.1.1.1
nslookup www.teamtavern.net 1.1.1.1
```

The old site is down already, so nothing is lost by switching before the new
one is up, and Caddy's first try at the certificates then succeeds.

## 4. Postgres 18: restore, import, rename

```bash
cd ~/team-tavern && set -a && . ./.env && set +a
docker compose up -d tt-postgres
until docker exec tt-postgres pg_isready -h localhost -U "$POSTGRES_USER" >/dev/null 2>&1; do sleep 1; done

docker exec -i tt-postgres pg_restore -U "$POSTGRES_USER" -d "$POSTGRES_DB" --exit-on-error \
    < ~/team_tavern-pg14.dump
docker exec tt-postgres vacuumdb -U "$POSTGRES_USER" -d "$POSTGRES_DB" --analyze-only
```

A restore carries no planner statistics, so `vacuumdb --analyze-only` gathers
them. The import reads `POSTGRES_USER` and `POSTGRES_DB` from the environment
set above, builds the new database beside the old one and prints its report:

```bash
cd ~/relaunch && bash redesign/import/import.sh team_tavern_relaunch 2>&1 | tee ~/relaunch/report.txt
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

The import leaves the old site's data inside the new database too, as the
schema `legacy` it maps from. Nothing on the site reads it, and it takes the
nightly backup, a gzipped dump mailed through SendGrid, past SendGrid's 30 MB
limit. `pg_dump` names every reference with its schema, so this counts what in
`public` depends on it, which must be 0, before dropping it:

```bash
docker exec tt-postgres pg_dump -U "$POSTGRES_USER" --schema-only --schema=public "$POSTGRES_DB" | grep -c 'legacy\.'
docker exec tt-postgres psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -v ON_ERROR_STOP=1 -c "drop schema legacy cascade"
```

It takes the extensions `pg_trgm` and `tablefunc` with it, which the old schema
had and nothing on the site uses.

## 5. Deploy

**Local:**

```bash
./deploy-release.sh "$new"
```

It uploads `release/` into `~/team-tavern`, beside the `.env`, and brings the
whole stack up on the imported database. `docker logs tt-caddy` shows
`certificate obtained successfully` for `teamtavern.net` and
`www.teamtavern.net`.

## 6. Check it

```bash
docker logs tt-node          # npm's install, then nothing: the server logs only errors
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

Add `cron.txt`'s line to the crontab with `crontab -e`, and run the backup once
by hand, which prints nothing and mails the dump:

```bash
cd ~ && set -a && . ~/team-tavern/.env && set +a
bash ~/team-tavern/backup-database.sh
```

The worker's first run is an hour after node starts. `docker logs tt-node` then
shows no `Error in the period worker`, and the posts in their last week have
their `expiry` notifications:

```bash
docker exec tt-postgres psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -c \
    "select kind, count(*), min(created), max(created) from notification group by kind"
```

Every imported address is unconfirmed, so that run emails only players who have
confirmed since.

## 7. The relaunch email

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

While nothing on the new site is worth keeping, **old:**

```bash
docker start node renderready caddy
crontab -e     # take the # off the backup line
```

and at Namecheap point both `A` records back at 67.207.77.252. The Droplet's
Caddy still holds its certificates. Whatever players did on the new site in
between is lost.

## Once it holds

On the new server, after a week or so:

```bash
rm -rf ~/relaunch ~/team_tavern-pg14.dump
docker exec tt-postgres dropdb -U "$POSTGRES_USER" "${POSTGRES_DB}_legacy"
```

On DigitalOcean, destroy the Droplet and release its reserved IP, which is
billed while it is assigned to nothing. The dump in `~/relaunch-backup` is then
the one copy of the old site.

On the Discord app, remove the old site's redirect URIs, `/register` and
`/preboarding/register` on each origin, and keep the `/signin` ones. In the
repo, delete `redesign/import/`, which addresses rows only the old production
had, and `RelaunchEmail/` with its lines in `build-server.sh`.
`TablesBase.sql` and `TablesCurrent.sql` are already the same file.
