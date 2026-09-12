# Smoke specs

Status: resolved

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

Landed as `test-playwright/integration/smoke.spec.ts`, four tests sharing one
`game` constant that carries valorant's handle, seeded nickname, title and field
keys. `integration/.gitkeep` is gone.

One departure, and it is the suite earning its keep on the first run. The
content-type assertion failed: the API answered `text/plain; charset=utf-8`.
The cause is not Caddy mislabelling it but the server never labelling it at all
— `Perun.Response` writes only the headers it is handed, Jarilo handed it none,
and Caddy sniffed the body. Rather than relax the assertion to match, the
header now exists: `BodyRouter` gains a `responseContentType`, `Nothing` for
`NoBody` and `application/json` for `JsonBody`, and `Jarilo.Router.Response`
puts it on every routed response. The route type already decides how the body
is serialized, so it is the honest place to name the media type. Responses with
no body stay unlabelled. That change is `eda5398` in the sibling
`purescript-bklaric`, not in this repo, and this suite does not pass without
it.

Three things about the fixtures that shaped the file.

The `request` fixture inherits every `use` option, `userAgent` included —
`runBeforeCreateRequestContext` copies the combined context options onto
`request.newContext()`. The `integration` project spreads
`devices["Desktop Chrome"]`, so "request it normally" in the fourth check
really is a browser user agent and needs no header, and `baseURL` and
`ignoreHTTPSErrors` arrive the same way. The ticket asked for
`ignoreHTTPSErrors` to be set; the config already sets it, and repeating it in
the spec would only matter for a hand-rolled `request.newContext()`.

A per-request `headers["user-agent"]` overrides that, applied after the context
default. That is how the first two checks get their user agents.

`page` would otherwise match the prerender matcher, because headless Chromium's
own user agent contains `HeadlessChrome`. The device descriptor replaces it, so
the browser check gets `/index.html` and the real SPA rather than the shell.

The bot check reads three things out of the HTML: the title, the seeded
nickname, and `Showing 1 - 1 out of 1 players` from the comment above, so the
nickname has to appear in a rendered listing rather than merely somewhere on
the page. The fourth check asserts each seeded field's key and label in
ordinal order rather than keys alone.

The title assertion is the exact
`Players / LFG / LFT - Valorant Team Finder | TeamTavern`, which
`Client/Pages/Profiles.purs` builds from the game's short title. That
distinguishes the rendered page from both the shell's
`Esports Team Finder | TeamTavern` and the degraded path, which falls back to
the lowercase handle. Per the note above, nothing asserts on an asset URL, a
canonical link, or a URL followed out of the bot HTML.

Verified: `npm run typecheck` clean; `npm test` on `5 passed` in 16-22s across
three runs, each preceded by the `down -v` reseed. The ticket's red check ran
for real — deleting `not path /api/*` from `base.Caddyfile` fails exactly the
first two and leaves the other two passing, and the captured bot HTML shows
`There has been an error loading the game. Please try again later.`, which is
the prerenderer's own API call being answered with the shell. Restoring the
line goes green again. `curl` confirms `Content-Type: application/json` on
`/api/games/valorant`. The development stack stayed up throughout, still serves
`https://localhost`, and its row counts are unchanged at 11 games, 31196
players and 26881 player profiles.
