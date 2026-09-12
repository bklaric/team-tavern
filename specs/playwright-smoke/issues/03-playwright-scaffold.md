# Playwright scaffold

Status: resolved

The runner, config and stack boot, with no assertions yet.

## To do

- Add `@playwright/test`, `docker-compose` and `@types/node` to `devDependencies`, and `"test": "playwright test"` to `scripts`.
- Add `playwright.config.ts` with two projects: `stack-setup` matching `test-playwright/stack.setup.ts`, and `integration` in `test-playwright/integration/` depending on it, one worker, HTML reporter, trace on first retry. Follow the config in `enhanced-image-viewer`.
- `stack.setup.ts` runs `down -v` then `up` on the test compose project, then polls `/api/games` through the test Caddy port until it answers, with a 60 second deadline.
- Add a `test-playwright/tsconfig.json` and git-ignore `playwright-report/` and `test-results/`.
- Add a Build and verify entry to `CLAUDE.md` for `npm test`.

## Done when

`npm test` boots the test stack, runs zero specs, and exits 0.

## Comments

Landed as `playwright.config.ts`, `test-playwright/stack.setup.ts` and
`test-playwright/tsconfig.json`, with `playwright-report/` and `test-results/`
git-ignored and `npm test` documented under Build and verify.

Four departures from the ticket above.

`typescript` joins the three devDependencies the ticket lists, behind a
`typecheck` script that Build and verify lists beside `npm test`, because
Playwright transpiles the specs without ever typechecking them and nothing else
in the repo would catch a type error. It resolves to 7.x, which takes
`module: "node16"` and this config unchanged.

`stack.setup.ts` checks that `dist-client/index.html` and `dist-server/server.js`
exist before it calls compose. Both stacks serve those out of the repo, so a
stack booted without them answers nothing, and the readiness wait is otherwise
the only thing that notices — sixty seconds later and without naming the cause.

The readiness wait is `expect(...).toPass()` rather than the deadline loop in
`enhanced-image-viewer`. It needs no `timeout.ts` helper and prints the call log
on timeout. `expect.poll` does not work here: it awaits its callback outside the
try/catch, so the first refused connection aborts the poll instead of retrying
it.

The HTML reporter is configured `open: "never"`. Its default opens a report
server on a failure, which blocks the terminal the run came from.

Three things worth knowing for ticket 04.

Playwright exits 0 with an empty `integration/` because "no tests found" is gated
on the whole run rather than per project, and a project with no tests is dropped
without its `dependencies` being visited, so `stack-setup` still runs on its own.
`integration/.gitkeep` only exists so the directory the config names survives a
clone, which is why deleting it is on ticket 04.

The poll talks to 8443 over HTTPS rather than 8080, because the `localhost` site
block's HTTP redirect drops the shifted port and would send the request to the
development stack.

`trace: "on-first-retry"` is inert as the ticket specifies it, because `retries`
is 0 and nothing has a first retry. A trace comes from `npm test -- --retries=1`
on demand, which is cheaper than paying a second `down -v` and boot on every
failure.

Verified with the development stack up throughout: `npm test` exiting 0 on
`1 passed` with zero specs collected, five times over, taking between 4.8s and
15.1s against the ticket's 60 second deadline, so the in-container
`npm ci --omit=dev` that every `down -v` forces is not close to it. Also checked
`playwright test --list` collecting only the setup test, and collecting a
throwaway spec dropped in `integration/` under its own project, the run failing
with `Run ./build.sh before npm test` and touching no container when the bundles
are missing, `npm run typecheck` clean and failing on a planted type error,
`docker compose ls` resolving the project to `teamtavern-test`, the development
row counts (31196 players, 26881 profiles, 11 games) identical before and after,
and `git status` free of `playwright-report/` and `test-results/`.
