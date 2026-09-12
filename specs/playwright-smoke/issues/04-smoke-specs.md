# Smoke specs

Status: ready-for-agent

One spec file, `test-playwright/integration/smoke.spec.ts`, with the four checks from the spec. Each check uses a seeded game handle from the test database.

## To do

- **API under a headless user agent.** Request `/api/games/<handle>` with a user agent containing `HeadlessChrome`; assert `content-type` is JSON and the body has a `handle` equal to the request.
- **Bot gets listings.** Request `/games/<handle>/players` with the Googlebot user agent; assert the body contains a `<title>` naming the game and at least one seeded player's nickname.
- **Browser gets listings.** Open `/games/<handle>/players` in Playwright's Chromium; assert a seeded nickname is visible.
- **API under a browser user agent.** Request `/api/games/<handle>` normally; assert the response lists the game's seeded fields.

Delete `test-playwright/integration/.gitkeep`, which only holds the directory open
until this spec lands.

Use Playwright's `request` fixture for the three HTTP checks and `page` for the browser one. Set `ignoreHTTPSErrors` since the test Caddy uses a self-signed certificate.

## Done when

`npm test` runs four passing specs against the test stack, and reverting the `not path /api/*` line in `stacks/base.Caddyfile` makes the first two fail.

## Comments

Two things issue 02 turned up while making the bot path work locally.

`/games/<handle>` is the game landing page: marketing copy and a game-specific
`<title>`, no profiles. The seeded nickname lives on `/games/<handle>/players`,
which is what the two listing checks name. Measured on a cold test stack:
`/games/valorant` prerenders 14143 bytes with the right `<title>` and no
nickname; `/games/valorant/players` prerenders with `ValorantTester` and
"Showing 1 - 1 out of 1 players".

Rendertron injects a `<base href>` when the document has none, and
`Client/Script/Meta.purs` builds canonical and `og:url` from
`window.location.origin`. Under the local render origin those all read
`http://caddy`, a name that resolves only inside the compose network.
Assertions must read page content and never an asset URL, a canonical link, or
a URL followed out of the bot HTML.
