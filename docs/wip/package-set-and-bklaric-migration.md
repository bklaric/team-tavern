# WIP: package set upgrade and migration to the `bklaric` library

## Goal

Move the PureScript package set off its 2022 pin and migrate the project onto
the shared `bklaric` library at `../purescript-bklaric`.

**These are one task, not two.** The library pins its own package set, and
consuming it means agreeing with it. Doing them separately means working through
the same compile breakages twice.

Expect large, wide breakage. That is inherent to the task, not a sign of a wrong
turn.

## Target

```yaml
package:
  dependencies:
    - bklaric
    # ...
workspace:
  extraPackages:
    bklaric:
      path: ../purescript-bklaric
  packageSet:
    registry: 77.10.0
```

`registry: 77.10.0` is what the library and the actively maintained sibling
projects (`enhanced-image-viewer`) use. The library is consumed as a **local
path**, not a git or ssh dependency — this is the pattern in every sibling
project. (`chess-assistant` sits on 53.1.0 and is not a model to copy.)

Newer sets exist — 80.9.0 is current — but matching the library matters more
than being newest.

Set 77.10.0 declares compiler 0.15.15 while this project and the library both
pin purs **0.15.16**. That combination is fine: the library builds on exactly
it.

## Starting point

`spago.yaml` currently pins the legacy URL form, from June 2022, governing all
97 locked packages:

```yaml
workspace:
  packageSet:
    url: https://raw.githubusercontent.com/purescript/package-sets/psc-0.15.2-20220610/packages.json
```

Useful commands:

```bash
./node_modules/.bin/spago registry package-sets   # list available sets
./node_modules/.bin/spago upgrade --package-set 77.10.0 --migrate
```

## The library

`../purescript-bklaric`, package name `bklaric`. Actively developed. **180
modules, 110 hand-written FFI files.** purs 0.15.16 and spago 1.0.4, matching
this project.

Coverage:

- `JavaScript.Web.*` — DOM, Fetch, URL, WebStorage, Clipboard, SVG, File,
  WebWorkers, WebAnimations, Crypto
- `JavaScript.Node.*` — Http, Fs, Crypto, Stream, Buffer, Events, Net, Process,
  Timers, Zlib
- `JavaScript.Npm.*` — Pg, AwsSdk, FastCsv, Alwan, UAParserJs
- `JavaScript.Chrome.*`
- Root level — `Async`, `Bcrypt`, `Postmark`, `Wrapped`, `Yoga`, `Data.*`,
  `Record`

### Missing FFI goes in the library

Any web, node or npm binding that turns out to be missing belongs in
`../purescript-bklaric`, not here.

**Read `../purescript-bklaric/src/CLAUDE.md` before writing any FFI.** It is
long and specific, and the existing 110 bindings all follow it. Load-bearing
points: exports are manually curried **subject-last**; raw imports are
`_`-prefixed and deliberately unconstrained, with a public wrapper that adds the
class constraint, `cast`s unions and optionals, and converts `Nullable` →
`Maybe`; `export const` arrow lambdas only, 4-space indent, no semicolons;
opaque `foreign import data` handles rather than records; DOM inheritance
modelled as type classes.

Build the library from its own repo — the compiler is the only check, there are
no FFI tests.

The library binds `Promise` directly and does not use `Aff`. This matters less
than it sounds: both repos centre on the same `Async` (`ContT`/`ExceptT`) type,
and direct `Effect.Aff` use is 2 files here and 1 there.

## Module overlap

This project has **51 modules outside the `TeamTavern` namespace**. The library
shares their ancestry — `Async` in both has near-identical imports and the same
design — so this is a convergence, not a rewrite.

### 21 exist in the library, 16 of them byte-identical

Pure deletions, zero risk. Do these first for a fast, safe reduction in surface
area.

### 5 have diverged

Diff each and prefer the library's version, moving any genuinely needed local
behaviour upstream:

`Async` · `Bcrypt` · `Bcrypt.Async` · `Data.MultiMap` · `Postmark.Client`

### 30 exist only here

| Local modules | Disposition |
|---|---|
| `Perun.*` (7 modules) | **Move to the library** — see below |
| `Jarilo.*` (14 modules) | **Move to the library** — see below |
| `Browser.Fetch`, `Browser.Fetch.Response`, `Browser.Async.Fetch`, `Browser.Async.Fetch.Response` | → `JavaScript.Web.Fetch.Fetch` |
| `Postgres.Async.Pool`, `Postgres.Async.Query`, plus the `pg` extraPackage | → `JavaScript.Npm.Pg.{Client,Pool,Query,Result,Error}` |
| `Record.Extra` | **Delete** — collides, see below |
| `Debug` | **Delete** — collides, see below |
| `Sendgrid` | No library equivalent. Add as `JavaScript.Npm.Sendgrid` in the library. |

`src/Browser/Fetch.js` is a hand-edited FFI file using `globalThis.fetch`, not
generated — check it before deleting the `Browser.Fetch` modules.

### Two certain module collisions

The library depends on `record-extra` and `debug`, so both arrive transitively
the moment `bklaric` is added:

- `record-extra-5.0.1` provides `module Record.Extra`, colliding with
  `src/Record/Extra.purs`
- `debug-6.0.2` provides `module Debug`, colliding with `src/Debug.purs`

Delete both local files in the same step that adds the dependency. These are
duplicate-module errors, not warnings.

## Extracting Perun and Jarilo

Both are general-purpose web libraries that happen to live in this repo, and
both belong in the shared library. Both are **pure PureScript** — 21 modules,
zero `.js` files between them — so this is a lift, not an FFI rewrite. They sit
at the library root as their own namespaces, alongside `Postmark` and `Wrapped`.

**Perun** (HTTP server abstraction: `Server`, `Request`, `Response`, `Url`,
`Request.Body`, `Async.Server`, `Async.Request.Body`) imports no TeamTavern
module — it is already clean. It currently sits on the `nodey` extraPackage
(`Node.Http`, `Node.Buffer`, `Node.Errors`, `Node.Events`, `Node.Stream`,
`Node.Server`). In the library it should rebase onto `JavaScript.Node.*`, which
covers every one of those. Doing so is what makes `nodey` removable.

**Jarilo** (typed routing DSL: `Router.*`, `Shared.*`, `Serve`, `Fetch`,
`Types`) has one coupling to break first:

```
src/Jarilo/Serve.purs:17:  import TeamTavern.Server.Infrastructure.Log (logStamped)
```

`logStamped` is a generic "timestamp, then log" helper with nothing
project-specific about it — it only lives in a `TeamTavern` namespace. Move it
(with `datetimeFormat`, and `logt` if useful) to the library. The rest of
`TeamTavern.Server.Infrastructure.Log` — `logError` and `print` — stays, being
tied to `Terror` and `NodeError`.

Jarilo also imports `Record.Extra`, which is being deleted in favour of the
`record-extra` package. Check that the package's API covers what Jarilo uses
before deleting the local copy.

Jarilo further depends on `Browser.Fetch` and `Perun.*`, so sequence it after
both of those have moved.

## Other things in `spago.yaml`

**`jarilo` is vestigial.** It is declared in `extraPackages` but absent from
`dependencies` — `src/Jarilo/` provides those modules locally. Delete the entry
rather than making it resolve. Once Jarilo lands in the shared library, the
standalone `bklaric/purescript-jarilo` repo it points at is superseded too; it
is not checked out locally.

**Five extraPackages resolve over SSH.** `error`, `jarilo`, `nodey`, `pg` and
`undefined` use `ssh://git@github.com/bklaric/...`, pinned to git tags, and are
not governed by the package set. None is checked out locally, so resolving them
needs a working GitHub SSH key or a switch to `https://` — worth confirming
early, since it blocks the first build.

The migration removes most of them: `pg` is superseded by `JavaScript.Npm.Pg`,
`jarilo` is already unused, and `nodey` goes once Perun rebases onto
`JavaScript.Node.*`. That leaves `error` and `undefined`:

- `undefined` → the library's `Undefined` module is a direct replacement.
- `error` provides `Error.Class`, used in 4 modules here. The library has no
  `Error.Class`; its `JavaScript.Error` covers the same ground (`name`,
  `message`, `stack`, `cause`, `readError`) under a different shape, so this is
  an API shift rather than a drop-in.

**Halogen is the biggest exposure** — `halogen`, `halogen-css`, `halogen-hooks`,
`halogen-subscriptions`, `halogen-svg-elems`, `halogen-vdom` are all locked and
the client UI is built on them.

## Working environment

Windows 11, **Git Bash** (the build scripts are bash; `cmd.exe` and PowerShell
will not run them).

**Never run spago through `npx`, and never put `node_modules/.bin` on PATH
before invoking it.** Either makes spago resolve `purs` to `purs.cmd`, which
Node refuses to spawn, and the build dies with `Error: spawn EINVAL`. Always
`./node_modules/.bin/spago`. `build.sh` already handles this and explains why.

```bash
./build.sh      # spago build, then client and server bundles
./run-stack.sh  # postgres, node, rendertron, caddy
```

A full rebuild is 1122 modules and takes a few minutes. `rm -rf output` forces
one; spago hashes content, so `touch` does not.

`CLAUDE.md` in the repo root governs comments and docs: present tense,
describing current behaviour, with change history left to `git log`.

## Baseline

| | Value |
|---|---|
| Full build | 0 errors, 114 warnings |
| Warning mix | 88 wildcard types, 8 missing top-level signatures, rest unused/implicit imports |
| `dist-client/app.min.*.js` | 867,184 bytes |
| `dist-server/server.js` | 1,209,592 bytes |
| Locked packages | 97 |

## Verification

All of these pass before the work starts and must pass after:

```bash
newjs=$(grep -oE 'app\.min\.[a-z0-9]+\.js' dist-client/index.html | head -1)
curl -sk -o /dev/null -w 'root  %{http_code}\n' https://localhost/
curl -sk -o /dev/null -w "js    %{http_code}\n" "https://localhost/$newjs"
curl -sk -o /dev/null -w 'games %{http_code}\n' https://localhost/api/games
curl -sk -o /dev/null -w 'profs %{http_code}\n' 'https://localhost/api/games/dota2/players?page=1&timezone=Europe/Zagreb'
curl -sk -X POST https://localhost/api/sessions -H 'Content-Type: application/json' \
  -d '{"type":"password","value":{"emailOrNickname":"FishNetLover","password":"wrong"}}'
```

The last one returns `{"type":"wrongPassword","value":{}}` with HTTP 400. It is
the single best check: it proves the server bundle, Postgres and the native
bcrypt module all work together. `{"type":"unknownPlayer"}` means the nickname
is wrong, not that something broke. The site uses a self-signed Caddy cert,
hence `-k`.

## Suggested sequence

1. Branch. Confirm the `ssh://` extraPackages resolve, or switch them to
   `https://` — this blocks the first build.
2. Switch the package set to `registry: 77.10.0` and add the `bklaric` path
   dependency together.
3. Delete the 16 byte-identical modules, plus `Record.Extra` and `Debug`.
4. Reconcile the 5 diverged modules against the library.
5. Replace `Browser.Fetch*` with `JavaScript.Web.Fetch`, and `Postgres.Async.*`
   with `JavaScript.Npm.Pg`. Add `JavaScript.Npm.Sendgrid` to the library.
6. Extract Perun to the library, rebasing it from `nodey` onto
   `JavaScript.Node.*`. Drop `nodey` and `pg` from `extraPackages`.
7. Move `logStamped` to the library, then extract Jarilo. Drop `jarilo` from
   `extraPackages`.
8. Resolve `error` and `undefined` against the library's `JavaScript.Error` and
   `Undefined`.
9. Rebuild to 0 errors and re-run the verification recipe.

Steps 6 and 7 are ordered: Jarilo depends on both Perun and `Browser.Fetch`.

Compile errors will come in waves. The 0-error/114-warning baseline is the
signal for being back to even.

## Repo state

The working tree carries uncommitted work on `master` at `d17de7d8`: `README.md`,
`CLAUDE.md`, `build.sh` and `.gitattributes` are new; `package.json`,
`package-lock.json`, `spago.lock`, `src/Browser/Fetch.js`,
`stacks/docker-compose.yml`, `build-client.sh` and `build-server.sh` are
modified. Commit or stash before starting.

## Unrelated open item

`stacks/.env` is committed and contains the Postgres password and a SendGrid API
key. Rotating those and untracking the file is independent of this task.
