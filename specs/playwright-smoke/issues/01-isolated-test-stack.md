# Isolated test stack

Status: resolved

A second compose project for tests that leaves the development stack and its database alone.

## To do

- Add `stacks/test.env` with its own `POSTGRES_DB`, a Postgres data volume that is a named volume rather than a host directory, and distinct host ports for Caddy.
- Add a compose override or a separate `stacks/docker-compose.test.yml` that uses that env file and a distinct project name, so `docker compose -p teamtavern-test` never collides with the development containers.
- Seed the database on boot from `TablesBase.sql`, the migrations in date order, and `Seed/`, in that order. Add at least one player profile per seeded game so listing pages have content to assert on.
- Document the two stacks side by side in `CLAUDE.md` under Running the stack.

## Done when

`docker compose -p teamtavern-test up` and `down -v` can run while the development stack is up, and the development database is unchanged afterwards.

## Comments

Landed as `stacks/test.env`, `stacks/test.Caddyfile`,
`stacks/docker-compose.test.yml` and `stacks/test-seed/`, with the two stacks
documented side by side under Running the stack in `CLAUDE.md`.

The seed departs from the plan above. `TablesBase.sql` plus the migrations does
not build an empty database: `2023-03-13-overwatch-2.sql` inserts field options
against field id 31 and `2023-04-07-account-trackers.sql` reads game ids by
handles no seed file carries, so both fail on a fresh volume. The migrations
repair production rows, so replaying them is meaningless anyway. The seed
applies `TablesCurrent.sql` instead, then `Seed/`, then a new
`stacks/test-seed/players.sql` that derives one player and one profile per row
of `game`.

That exposed a second thing: `TablesCurrent.sql` had `field.ilk` as `integer`,
but `2023-04-01-improved-filtering.sql` changes it to `text` and the production
database confirms `text`. The game seeds insert `'single'` and `'multi'`, so the
stale type broke the seed. Corrected in the same change.

Verified with the development stack running throughout: both stacks up at once,
`https://localhost:8443/api/games/<handle>/players` returning a seeded nickname
for all four of apex, hots, r6s and splitgate, zero psql errors in the postgres
log, `https://localhost` still serving the development site, and the development
row counts (31196 players, 26881 profiles, 11 games) identical before and after
`down -v`, which removed every test volume.
