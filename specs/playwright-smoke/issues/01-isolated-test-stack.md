# Isolated test stack

Status: ready-for-agent

A second compose project for tests that leaves the development stack and its database alone.

## To do

- Add `stacks/test.env` with its own `POSTGRES_DB`, a Postgres data volume that is a named volume rather than a host directory, and distinct host ports for Caddy.
- Add a compose override or a separate `stacks/docker-compose.test.yml` that uses that env file and a distinct project name, so `docker compose -p teamtavern-test` never collides with the development containers.
- Seed the database on boot from `TablesBase.sql`, the migrations in date order, and `Seed/`, in that order. Add at least one player profile per seeded game so listing pages have content to assert on.
- Document the two stacks side by side in `CLAUDE.md` under Running the stack.

## Done when

`docker compose -p teamtavern-test up` and `down -v` can run while the development stack is up, and the development database is unchanged afterwards.
