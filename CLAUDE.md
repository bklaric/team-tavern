# TeamTavern

A team-finding site for gamers. One PureScript codebase produces both the
browser SPA and the Node API server; both compile with a single `spago build`
and share their HTTP contract as PureScript types.

## Layout

```
src/TeamTavern/
  Routes/         the HTTP contract, shared by client and server
  Server/         Node API (Jarilo handlers over Postgres)
  Client/         Halogen SPA, its styles and static assets
  Shared/         static data both sides need (countries, languages, timezones)
  Database/       SQL schema and seed data
stacks/           docker compose, Caddyfiles, env files and the test seed
test-playwright/  the Playwright suite and the stack boot it runs first
test/             a stub; nothing runs it
```

Generated, never edited, all git-ignored: `output/` (compiled PureScript),
`dist-client/`, `dist-server/`, `dist-test/`, `.spago/`, and Playwright's `playwright-report/`
and `test-results/`.

## Environment

- **Node and npm** are pinned by Volta in `package.json`; with Volta installed
  the right versions are picked up automatically. The `node` service in
  both compose files under `stacks/`, and the test stack's `discord` service,
  pin the same Node version for their containers, and nothing enforces
  agreement, so change all four together.
- **purs, spago, sass, esbuild and Playwright** come from `devDependencies`, so
  setup is `npm install` plus, for the browser Playwright drives,
  `./node_modules/.bin/playwright install chromium`, which downloads Chromium
  into a per-user cache outside the repo. No global installs.
- **Git Bash** on Windows. The scripts are bash and do not run under
  `cmd.exe` or PowerShell. **OpenSSL** must be on PATH for the cache-busting
  asset hash in `build-client.sh`; Git Bash ships it.
- **Docker Desktop** (WSL2 backend on Windows) for the local stack.

## Build and verify

```bash
npm install         # once
spago build         # compile everything into output/
./build.sh          # spago build + build-client.sh + build-server.sh
./run-stack.sh      # docker compose up: postgres, node, renderready, caddy
./deploy-server.sh  # rebuild the server bundle and restart the node container
npm test            # boot the test stack and run the Playwright suite
npm run typecheck   # tsc over test-playwright/ and playwright.config.ts
```

Bare `spago` and `purs` work because Volta shims them and, since both are
dependencies in `package.json`, runs the project-local copies. Without Volta,
call `./node_modules/.bin/spago` as the scripts do. Never `npx spago`, and
never put `node_modules/.bin` on PATH before running spago: spago then finds
the `purs.cmd` shim, which Node refuses to spawn, and the build dies with
`spawn EINVAL`.

`npm test` is Playwright, configured in `playwright.config.ts`.
`test-playwright/stack.setup.ts` takes the test stack down with `-v`, brings it
back up and waits for `/api/games` to answer, and the specs in
`test-playwright/integration/` then run against it. The stack serves
`dist-client/`, `dist-server/` and `dist-test/` out of the repo, so `./build.sh`
has to have run first; the setup says so rather than letting the wait time out.

A spec drives the site through the browser, as a player would: it sets up what it
needs through the pages and asserts on what they show, never by calling the API
or the database. What the API answers can be right while the page shows the wrong
thing, and only the page is what players see.

Nothing typechecks the suite on the way to running it, since Playwright strips
the types without reading them, so `npm run typecheck` is a separate step.

A change is verified when `spago build` reports no errors, `npm test` passes, and
the affected page or endpoint behaves in the running stack.

`build-client.sh` compiles Sass and bundles the client into `dist-client/`
under hashed file names. `build-server.sh` bundles the server into
`dist-server/server.js` with `bcrypt`, `pg` and `@sendgrid/mail` left
external, and copies the root `package.json` beside it; the container installs
that with `--omit=dev`, so the build toolchain never enters the image. It also
bundles `DiscordStub/Main.purs` into `dist-test/discord-stub.js`, which only the
test stack runs.

## Running the stack

Two compose projects, and they can run at once. The development stack keeps its
data in host directories next to the repo; the test stack keeps its in named
volumes, so `down -v` throws the database away and the next boot seeds a fresh
one. Both serve the same `dist-client/` and `dist-server/`, so `./build.sh` has
to have run either way.

Both also answer to `http://caddy` on their compose network, and that name is
the render origin passed to `base.Caddyfile`. Renderready's browser has to fetch
the site itself, and inside that container `localhost` is renderready. Neither
compose file pulls a renderready image: the service is built from the upstream
git tag, so the first `up` on a machine builds it, and that takes a while.

|               | Development                 | Test                             |
| ------------- | --------------------------- | -------------------------------- |
| Compose file  | `stacks/docker-compose.yml` | `stacks/docker-compose.test.yml` |
| Env file      | `stacks/.env`               | `stacks/test.env`                |
| Project name  | default                     | `teamtavern-test`                |
| Site          | <http://localhost:8000>     | <http://localhost:8080>          |
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

Both local stacks serve plain HTTP, on the host port `CADDY_HTTP_PORT` names:
8000 in `stacks/.env`, 8080 in `stacks/test.env`. Production shares
`docker-compose.yml`, leaves the variable unset and takes ports 80 and 443,
where `production.Caddyfile` serves HTTPS. Session cookies drop `Secure` under
`DEPLOYMENT=local`, and browsers treat `http://localhost` as a secure context,
so nothing on the site needs HTTPS locally.

`stacks/.env` is committed. The Postgres credentials in it are real, but the
database is reachable only from inside the compose network, so they are
usable only by someone already on the server. The SendGrid key is a
placeholder; the real one lives in the production `.env` on the server and is
not in the repo.

### The test stack

`stacks/test.env` is self-contained: its own database name, its own throwaway
credentials and the Caddy host port. It names no host directory, so the
stack carries nothing between runs. Caddy loads `stacks/test.Caddyfile`, which
the compose file mounts directly rather than selecting by `ENVIRONMENT`.

```bash
docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml up -d
docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml down -v
```

The compose file names the project itself, so `-p teamtavern-test` is already
implied. `--env-file` is not: the port variable is declared required, so
without it compose refuses to start rather than binding an arbitrary host port.
`npm test` runs both commands itself, so a test run takes this stack down and
reseeds it from whatever state it was in.

The test stack has no Discord. Its `discord` service runs
`dist-test/discord-stub.js`, and `DISCORD_API_URL` in `test.env` points the
server at it. The stub answers the user endpoint with whatever user the access
token names, the URI-encoded JSON of that user, so a spec can sign up and sign in
with Discord as anyone, verified email or not. The browser half of the flow never
reaches Discord either: `test-playwright/integration/sign-in.spec.ts` answers the
pages' redirect to Discord's authorize URL itself, sending the browser straight
back with such a token, and checks the scope and redirect URI the page asked for.

What no test reaches is Discord itself: the redirect URIs registered on the
Discord app and the real user endpoint. Before a deploy that touches sign-in,
check them by hand against the development stack, whose `http://localhost:8000`
pages are registered redirect URIs, with a real Discord account:

1. Create an account with Discord at <http://localhost:8000/register>. It lands on
   onboarding, and Change email on the account page shows the Discord address.
2. Sign out, sign in with Discord at <http://localhost:8000/signin>, and land signed in.
3. `docker logs node` shows no Discord errors for either.

`stacks/test-seed/seed.sh` builds the database on the first boot of the Postgres
volume, which is why `down -v` rather than `down` is what resets it. It applies
`TablesCurrent.sql`, then `Seed/`, then `stacks/test-seed/players.sql`. That
last one gives every seeded game one player and one profile, so the listing
pages have a row to assert on; the nickname is the handle title-cased with
`Tester` after it, so `apex` gets `ApexTester`, the email is
`apex@example.com`, and the password is `tester-password`. `Seed/Games/` carries all eleven
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

`spago.yaml` pins the package set (`registry: 77.10.0`) and three extra
packages, each resolved from a sibling checkout next to the repo:

| Package          | Checkout                        | Branch                 |
| ---------------- | ------------------------------- | ---------------------- |
| `bklaric`        | `../purescript-bklaric`         | `main`                 |
| `untagged-union` | `../purescript-untagged-union`  | `recursive-castable`   |
| `yoga-json`      | `../purescript-yoga-json`       | `fix-variant-decoding` |

The last two are forks that `bklaric` builds against. A path package's own
workspace does not carry over, so team-tavern names them itself. The build
compiles whatever branch each checkout has out, so a checkout on another
branch builds against different code.

`bklaric` supplies `Async`, `AsyncV`, `Jarilo`, `Data.Validated`, `Bcrypt`,
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
user agents to renderready, and serves everything else from `dist-client/` with
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
  `stacks/.env`. `DISCORD_API_URL` is optional and defaults to Discord's own
  API; only `stacks/test.env` sets it.

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

`Database/` is plain SQL. `TablesCurrent.sql` is the schema, and it is what the
test stack builds from. `TablesBase.sql` is the schema production and the
development database had before the scripts in `Migrations/`, so
`TablesBase.sql` with those scripts applied in date order gives
`TablesCurrent.sql`. `Seed/` holds the region rows and one file per game, each
carrying that game's fields, field options and trackers. Only the test stack
runs them, on every fresh boot.

A schema change is a dated script in `Migrations/`, one transaction, and the
same edit to `TablesCurrent.sql`. The script is applied by hand to the
`postgres` container of the development stack and to production. Before it
goes out, apply `TablesBase.sql` and the scripts to one scratch database and
`TablesCurrent.sql` to another, and `pg_dump --schema-only` both: the dumps
must not differ. Once a script has run everywhere, `TablesBase.sql` is replaced
with `TablesCurrent.sql` and the script is deleted. A migration kept past that
point rots: it may address rows by ids only production has, so nothing can
replay it and nothing catches it going stale.

`Seed/Games/` is the production game catalogue, so it is checked against the
development database rather than written freehand.

A game is its seed file plus one cover, a 600x900 WebP at
`Client/Static/Images/Games/<handle>.webp`, served as `/images/games/<handle>.webp`.
The cover is the only per-game asset: the header dropdown, the home page grid and
the onboarding picker all show it, and nothing shows a game icon. Every seeded
game must have one; nothing generates a stand-in, and `games.spec.ts` fails on a
home page tile whose cover does not load at that size. Steam's
`library_600x900_2x.jpg` is that shape for games on Steam; SteamGridDB carries
the same shape for the rest.

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
