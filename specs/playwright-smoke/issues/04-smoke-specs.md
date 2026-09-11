# Smoke specs

Status: ready-for-agent
Blocked by: 02, 03

One spec file, `test-playwright/integration/smoke.spec.ts`, with the four checks from the spec. Each check uses a seeded game handle from the test database.

## To do

- **API under a headless user agent.** Request `/api/games/<handle>` with a user agent containing `HeadlessChrome`; assert `content-type` is JSON and the body has a `handle` equal to the request.
- **Bot gets listings.** Request `/games/<handle>` with the Googlebot user agent; assert the body contains a `<title>` naming the game and at least one seeded player's nickname.
- **Browser gets listings.** Open `/games/<handle>/players` in Playwright's Chromium; assert a seeded nickname is visible.
- **API under a browser user agent.** Request `/api/games/<handle>` normally; assert the response lists the game's seeded fields.

Use Playwright's `request` fixture for the three HTTP checks and `page` for the browser one. Set `ignoreHTTPSErrors` since the test Caddy uses a self-signed certificate.

## Done when

`npm test` runs four passing specs against the test stack, and reverting the `not path /api/*` line in `stacks/base.Caddyfile` makes the first two fail.
