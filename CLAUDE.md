# TeamTavern

A team-finding site for gamers. One PureScript codebase produces both the
browser SPA and the Node API server; both compile with a single `spago build`
and share their HTTP contract as PureScript types.

## Layout

```
src/TeamTavern/
  Routes/     the HTTP contract, shared by client and server
  Server/     Node API (Jarilo handlers over Postgres)
  Client/     Halogen SPA, its styles and static assets
  Shared/     static data both sides need (countries, languages, timezones)
  Database/   SQL schema and seed data
stacks/       docker compose, Caddyfiles, env files and the test seed
test/         a stub; the compiler is the only check
```

Generated, never edited, all git-ignored: `output/` (compiled PureScript),
`dist-client/`, `dist-server/`, `.spago/`.

## Environment

- **Node and npm** are pinned by Volta in `package.json`; with Volta installed
  the right versions are picked up automatically. The `node` service in
  both compose files under `stacks/` pins the same Node version for the
  container, and nothing enforces agreement, so change all three together.
- **purs, spago, sass and esbuild** come from `devDependencies`, so
  `npm install` is the only setup step. No global installs.
- **Git Bash** on Windows. The scripts are bash and do not run under
  `cmd.exe` or PowerShell. **OpenSSL** must be on PATH for the cache-busting
  asset hash in `build-client.sh`; Git Bash ships it.
- **Docker Desktop** (WSL2 backend on Windows) for the local stack.

## Build and verify

```bash
npm install         # once
spago build         # compile everything into output/; this is the check
./build.sh          # spago build + build-client.sh + build-server.sh
./run-stack.sh      # docker compose up: postgres, node, rendertron, caddy
./deploy-server.sh  # rebuild the server bundle and restart the node container
```

Bare `spago` and `purs` work because Volta shims them and, since both are
dependencies in `package.json`, runs the project-local copies. Without Volta,
call `./node_modules/.bin/spago` as the scripts do. Never `npx spago`, and
never put `node_modules/.bin` on PATH before running spago: spago then finds
the `purs.cmd` shim, which Node refuses to spawn, and the build dies with
`spawn EINVAL`.

There are no tests. A change is verified when `spago build` reports no errors
and the affected page or endpoint behaves in the running stack.

`build-client.sh` compiles Sass and bundles the client into `dist-client/`
under hashed file names. `build-server.sh` bundles the server into
`dist-server/server.js` with `bcrypt`, `pg` and `@sendgrid/mail` left
external, and copies the root `package.json` beside it; the container installs
that with `--omit=dev`, so the build toolchain never enters the image.

## Running the stack

Two compose projects, and they can run at once. The development stack keeps its
data in host directories next to the repo; the test stack keeps its in named
volumes, so `down -v` throws the database away and the next boot seeds a fresh
one. Both serve the same `dist-client/` and `dist-server/`, so `./build.sh` has
to have run either way.

Both also answer to `http://caddy` on their compose network, and that name is
the render origin passed to `base.Caddyfile`. Rendertron's browser has to fetch
the site itself, and inside that container `localhost` is rendertron.

|               | Development                 | Test                             |
| ------------- | --------------------------- | -------------------------------- |
| Compose file  | `stacks/docker-compose.yml` | `stacks/docker-compose.test.yml` |
| Env file      | `stacks/.env`               | `stacks/test.env`                |
| Project name  | default                     | `teamtavern-test`                |
| Site          | <https://localhost>         | <https://localhost:8443>         |
| Database      | `team_tavern`               | `team_tavern_test`               |
| Postgres data | host directory              | named volume, seeded on boot     |

### The development stack

`stacks/.env` configures docker compose. Besides the database and SendGrid
credentials it names four host directories that are bind-mounted into the
containers and live next to the repo, not in it:

| Variable               | Contents                          |
| ---------------------- | --------------------------------- |
| `TEAMTAVERN_PATH`      | this repo                         |
| `POSTGRES_DOCKER_PATH` | Postgres data, persists across restarts |
| `POSTGRES_BACKUP_PATH` | database backups                  |
| `CADDY_DOCKER_PATH`    | Caddy's `data` and `config` dirs  |

On Windows these use the `/c/Users/...` form. `ENVIRONMENT` in the same file
selects which `stacks/<name>.Caddyfile` Caddy loads.

The site is served at <https://localhost> with a Caddy-issued self-signed
certificate, so expect a browser warning, or `curl -k`.

`stacks/.env` is committed. The Postgres credentials in it are real, but the
database is reachable only from inside the compose network, so they are
usable only by someone already on the server. The SendGrid key is a
placeholder; the real one lives in the production `.env` on the server and is
not in the repo.

### The test stack

`stacks/test.env` is self-contained: its own database name, its own throwaway
credentials and the two Caddy host ports. It names no host directory, so the
stack carries nothing between runs. Caddy loads `stacks/test.Caddyfile`, which
the compose file mounts directly rather than selecting by `ENVIRONMENT`.

```bash
docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml up -d
docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml down -v
```

The compose file names the project itself, so `-p teamtavern-test` is already
implied. `--env-file` is not: the port variables are declared required, so
without it compose refuses to start rather than binding arbitrary host ports.

`stacks/test-seed/seed.sh` builds the database on the first boot of the Postgres
volume, which is why `down -v` rather than `down` is what resets it. It applies
`TablesCurrent.sql`, then `Seed/`, then `stacks/test-seed/players.sql`. That
last one gives every seeded game one player and one profile, so the listing
pages have a row to assert on; the nickname is the handle title-cased with
`Tester` after it, so `apex` gets `ApexTester`. `Seed/Games/` carries all eleven
production games, so every game handle the site serves has a page with content.
A cold boot answers on the API within a few seconds.

### Expected noise

None of it is a bug to fix:

- **`API key does not start with "SG."`** on node startup: `SENDGRID_API_KEY`
  in `stacks/.env` is a placeholder. Outbound email is off; everything else
  works.
- **`npm warn install-scripts ... bcrypt`** on node startup: npm skips the
  unapproved install script, and bcrypt does not need it because it ships
  Node-API prebuilds that `node-gyp-build` picks at require time.
- **`$'\r': command not found`**, or a container that cannot reach
  `postgres`: a file has CRLF endings. See the line-ending rule under Code
  style.

## Dependencies

`spago.yaml` pins the package set (`registry: 77.10.0`) and one extra package:
`bklaric`, resolved from the sibling checkout at `../purescript-bklaric`. That
library supplies `Async`, `AsyncV`, `Jarilo`, `Data.Validated`, `Bcrypt`,
`Log` and every `JavaScript.*` FFI module (`JavaScript.Npm.Pg`,
`JavaScript.Web.Fetch`, `JavaScript.Node.*`, ...). A missing or stale sibling
checkout is the first thing to suspect when an import from those namespaces
fails. Its own `src/CLAUDE.md` governs changes made there.

The npm side is three runtime packages (`bcrypt`, `pg`, `@sendgrid/mail`) that
the server bundle leaves external, plus the build toolchain in devDependencies.

## How a request flows

1. **Route** in `Routes/<Area>/<Name>.purs`: a Jarilo type such as
   `PostJson_ (Literal "players") RequestContent ==> (NoContent ! BadRequestJson BadContent ! ...)`,
   together with the request, success and error payload types. Error payloads
   are `Variant`s. `Routes/All.purs` joins every route into `AllRoutes`, keyed
   by name. `Routes/Shared/` holds payload types reused across routes.
2. **Server handler** in `Server/<Area>/<Name>.purs`, wired by name in the
   record `Server/Main.purs` passes to `serve (Proxy :: _ AllRoutes)`. Adding a
   route means adding it to `AllRoutes` and to that record; the compiler
   rejects a mismatch.
3. **Client call** through `Client/Shared/Fetch.purs` (`fetchBody`,
   `fetchPath`, ...), which takes the route type as a proxy, prefixes `/api`
   and sends cookies. The response comes back as the same `Variant` the route
   declares, so pages `match` / `onMatch` on it.

In the running stack Caddy proxies `/api/*` to the node container, sends bot
user agents to rendertron, and serves everything else from `dist-client/` with
an `index.html` fallback for SPA paths.

## Server conventions

- A handler runs in `Async (TerrorVar responses)` and is wrapped in
  `sendResponse "<heading>"`, which logs the error lines and turns the error
  into the HTTP response variant. Signed-in checks come from
  `Server/Infrastructure/EnsureSignedIn*.purs` and `CheckSignedIn.purs`.
- Errors are `Terror error (Array String)` from
  `Server/Infrastructure/Error.purs`: the typed error the client sees plus
  free-text lines for the log. Validation accumulates with `Validated` and
  `NonEmptyArray` of `Variant`s; see the `Terror*` aliases in that module.
- A handler with several steps gets a sibling folder of the same name
  (`Server/Player/Register/AddPlayer.purs`, `.../ValidateRegistration.purs`),
  one step per module. `Server/<Area>/Domain/` holds validated value types
  (`Nickname`, `Password`, `Hash`); `Server/<Area>/Infrastructure/` holds
  helpers shared across that area's handlers.
- SQL is written inline as `Query """ ... """` with positional `$n` parameters
  supplied through `:` and `:|`, and rows are decoded with Yoga.JSON `read`.
  Postgres errors are mapped to typed errors by constraint name
  (`player_nickname_key` and friends), so a new unique constraint needs a
  matching branch where it can fire.
- Configuration is environment variables read once in `Server/Main.purs`
  (`PG*`, `SENDGRID_API_KEY`, `DEPLOYMENT` = `local` | `cloud`), supplied by
  `stacks/.env`.

## Client conventions

- `Client/Main.purs` mounts `Client/Router.purs` on `#spa-teamtavern`. The
  router maps the pathname to a `State` constructor and renders the page for
  it. In-app links keep an `href` for crawlers and intercept the click with
  `navigateWithEvent_` from `Client/Script/Navigate.purs`, which pushes
  history and re-routes without a reload. A new page is a new `State` case
  plus a `render` and path match.
- `Client/Pages/` are routed pages, `Client/Components/` are reusable pieces,
  `Client/Script/` are browser helpers (each `.js` is the FFI for the `.purs`
  beside it), `Client/Snippets/` are tiny HTML helpers such as `HS.class_`.
- Components run in `Async left` and are written either as `H.mkComponent`
  with `Action` / `State` / `handleAction`, or with Halogen Hooks. Match the
  neighbouring code; both are in use. Child slot types come from
  `Client/Shared/Slot.purs` (`Slot___`, `SlotQ__`, `Slot_O_`, ...).
- Styles are Sass. Every page or component with styling has a `.scss` next to
  its `.purs`, opens with `@use "../Style/Base" as *;` for the shared
  variables and placeholders, and is registered with a `@use` line in
  `Client/Style/Main.scss`. A stylesheet not listed there is not in the
  bundle. Classes are plain kebab-case strings (`primary-button`,
  `form-heading`) applied with `HS.class_`.
- `Client/Static/` (index.html, favicons, fonts, images, robots and sitemap)
  is copied verbatim by `build-client.sh`; adding a new directory there means
  adding a `cp` line to that script.

## Database

`Database/` is plain SQL. `TablesCurrent.sql` is the schema; `Seed/` holds the
region rows and one file per game, each carrying that game's fields, field
options and trackers. Only the test stack runs them, on every fresh boot.

A schema change edits `TablesCurrent.sql` and is applied by hand to the
`postgres` container of the development stack and to production. A change that
also has to transform rows already out there needs a migration script beside it,
deleted once it has run everywhere. A migration kept past that point rots: it
addresses rows by ids only production has, so nothing can replay it and nothing
catches it going stale.

`Seed/Games/` is the production game catalogue, so it is checked against the
development database rather than written freehand.

## Code style

- PureScript: 4-space indentation, `∀` for `forall`, explicit alphabetised
  import lists, and left-to-right pipelines with `#`, `<#>` and `>>=` rather
  than nested application. No formatter is configured; keep the style of the
  surrounding file.
- JavaScript FFI: one `.js` per `.purs`, `export const` arrow chains, curried
  and thunked for effects, as in `Client/Script/`.
- Every text file is LF, normalized on the way in by `.gitattributes`; a stray
  CR breaks bash and docker compose.
- Commit subjects are imperative and capitalised, without a trailing period;
  the body says why.

## Writing comments and docs in this repo

- **Present tense only.** Comments and docs describe what the code does now. No
  "this previously imported X", no "changed when we upgraded to Y", no "added in
  May 2026", no "used to be a workaround for Z". Version numbers, dates, and the
  reason a line changed belong in the git log, not in the source.
- **Explain the non-obvious present, not the past.** A comment earns its place by
  saying something the code cannot: an invariant, a constraint imposed from
  outside, a reason the obvious approach does not work here. If the sentence only
  makes sense to someone who remembers the previous version, delete it.
- **Don't leave a comment where a name will do.** Prefer renaming the binding or
  extracting the expression over narrating it.
- **Don't document degraded behavior.** If a line only makes sense in conjunction
  with a bug or an upstream layer misbehaving, it is describing a regression to
  fix, not a contract to publish.
- **No changelog sections.** Nothing in this repo needs a "Recent changes" or
  "Migration notes" heading. `git log` is the changelog.

## Agent skills

### Issue tracker

Local markdown files under `specs/<feature-slug>/`. See `docs/agents/issue-tracker.md`.

### Triage labels

The five default triage labels, plus `resolved` for closing an issue. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` and `docs/adr/` at the repo root. See `docs/agents/domain.md`.
