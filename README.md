# TeamTavern

## Local development

### Prerequisites

- **Node + npm** — the versions are pinned by Volta in `package.json`. With
  Volta installed, the right ones are picked up automatically.
- **Docker Desktop** (WSL2 backend on Windows).
- **Git Bash** on Windows. The build scripts are bash scripts; they work fine
  under Git Bash, but not under `cmd.exe` or PowerShell.
- **OpenSSL** on PATH — `build-client.sh` uses it to generate the cache-busting
  asset hash. Git Bash ships with it.

purs, spago, sass and esbuild all come from `devDependencies` via
`node_modules/.bin`, so `npm install` is the only setup step and everyone builds
with the same pinned versions. Global installs are not required.

### Host directories

`stacks/.env` points at four directories that are bind-mounted into the
containers. They live next to the repo and are *not* part of it:

| Variable               | Contents                        |
| ---------------------- | ------------------------------- |
| `TEAMTAVERN_PATH`      | this repo                       |
| `POSTGRES_DOCKER_PATH` | Postgres data directory         |
| `POSTGRES_BACKUP_PATH` | database backups                |
| `CADDY_DOCKER_PATH`    | Caddy's `data` and `config` dirs |

On Windows these use the `/c/Users/...` form, not `C:\Users\...`.

### Build and run

```bash
npm install     # first time only
./build.sh      # compile PureScript, bundle client and server
./run-stack.sh  # bring up postgres, node, rendertron and caddy
```

The site is then served over HTTPS at <https://localhost> using a Caddy-issued
self-signed certificate, so expect a browser warning (or use `curl -k`).

`build.sh` runs three steps, which can also be run on their own:

- `./node_modules/.bin/spago build` — compiles PureScript into `output/`
- `./build-client.sh` — Sass + esbuild into `dist-client/`
- `./build-server.sh` — esbuild into `dist-server/`

To redeploy just the server after a change, `./deploy-server.sh` rebuilds the
server bundle and restarts the `node` container.

### Gotchas

**Do not run spago through `npx`, and do not put `node_modules/.bin` on PATH
before running it.** Either one makes spago resolve `purs` to the `purs.cmd`
shim, and since Node 20.12 `child_process.spawn` refuses to execute `.cmd`
files without `shell: true`. The build dies with:

```
Error: spawn EINVAL
```

Invoke it as `./node_modules/.bin/spago` instead — spago then resolves the real
`node_modules/purescript/purs.bin`, which is an ordinary `.exe`.

**Line endings.** `core.autocrlf=true` is the Windows default, but shell scripts
and `stacks/.env` break with CRLF — bash treats the trailing `\r` as part of the
command, and docker compose puts it inside `env_file` values. `.gitattributes`
forces LF for those files. If you see `$'\r': command not found` or a container
failing to reach `postgres`, check for a stray CR.

**Rendertron returns 400 locally.** Caddy rewrites bot requests to
`rendertron:3000/render/https://localhost/`, but inside the rendertron container
`localhost` is rendertron itself, so it can't reach Caddy. This only affects the
prerender path for bot user agents and is expected in local development.

**`API key does not start with "SG."`** on node startup just means
`SENDGRID_API_KEY` in `stacks/.env` is a placeholder. Outbound email won't work;
everything else does.

**`npm warn install-scripts ... bcrypt@6.0.0`** on node startup is expected. npm
declines to run install scripts unapproved, and bcrypt does not need its one:
it ships Node-API prebuilds, and `node-gyp-build` picks
`prebuilds/linux-x64/bcrypt.glibc.node` at require time. Node-API binaries are
ABI-stable, so the same prebuild works across Node majors without recompiling.

### Database

Postgres 14 data lives in `POSTGRES_DOCKER_PATH` and persists across restarts.
Schema and seed data are in `src/TeamTavern/Database/`:
`TablesBase.sql` / `TablesCurrent.sql`, `Migrations/` and `Seed/`.

## Known constraints

**The PureScript package set is pinned to `psc-0.15.2-20220610`.** That one line
in `spago.yaml` governs the version of every PureScript library in the project,
independently of the compiler and spago versions. Moving off it is what any
library modernization involves.

**Five `bklaric/purescript-*` packages are vendored as `extraPackages`.** They
resolve over `ssh://git@github.com`, so building without an SSH key that GitHub
accepts requires switching those URLs to `https://`.

**Local Node and container Node are pinned in two places.** Volta pins the local
version in `package.json`; the `node` service in `stacks/docker-compose.yml`
pins the image. Nothing enforces agreement between them, so change both together.

**`build-server.sh` copies the root `package.json` into `dist-server/`,**
devDependencies included. The container installs with `--omit=dev` so the build
toolchain stays out of it; the server bundle needs only `bcrypt`, `pg` and
`@sendgrid/mail`, the three esbuild `--external`s.

**Sass stylesheets use `@import`,** which Dart Sass 3.0 removes. There is an
automated migrator at <https://sass-lang.com/d/import>.

## Security note

`stacks/.env` is committed to the repository and contains the Postgres password
and a SendGrid API key. Those credentials should be rotated and the file moved
out of version control.
