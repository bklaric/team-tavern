# Playwright scaffold

Status: ready-for-agent
Blocked by: 01

The runner, config and stack boot, with no assertions yet.

## To do

- Add `@playwright/test`, `docker-compose` and `@types/node` to `devDependencies`, and `"test": "playwright test"` to `scripts`.
- Add `playwright.config.ts` with two projects: `stack-setup` matching `test-playwright/stack.setup.ts`, and `integration` in `test-playwright/integration/` depending on it, one worker, HTML reporter, trace on first retry. Follow the config in `enhanced-image-viewer`.
- `stack.setup.ts` runs `down -v` then `up` on the test compose project, then polls `/api/games` through the test Caddy port until it answers, with a 60 second deadline.
- Add a `test-playwright/tsconfig.json` and git-ignore `playwright-report/` and `test-results/`.
- Add a Build and verify entry to `CLAUDE.md` for `npm test`.

## Done when

`npm test` boots the test stack, runs zero specs, and exits 0.
