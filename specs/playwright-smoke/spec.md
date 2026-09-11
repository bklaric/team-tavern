# Playwright smoke suite

An end-to-end suite that boots the stack and checks the things a unit test cannot: that the API, the prerender path and the browser path each return what they should. Its first job is to guard against the prerender fault in `specs/prerender-fix`, which went unnoticed for sixteen months because nothing checked what bots receive.

## Shape

Mirror `enhanced-image-viewer`: a `test-playwright/` directory, TypeScript, `@playwright/test` and the `docker-compose` npm package as dev dependencies, a `stack-setup` project that boots the stack and waits for it, and an `integration` project that depends on it. The runner is `npm test`.

## Constraints

- **Isolated stack.** The development stack holds a restored production database and must not be touched. Tests run their own compose project with a throwaway Postgres seeded from `src/TeamTavern/Database/`, their own Caddy port, and their own env file.
- **Real bot path.** Rendertron resolves `localhost` to itself, so the bot request must reach the site by a name the rendertron container can resolve. The development Caddyfile serves that name and the bot rewrite targets it instead of the incoming host.
- **No production access.** Nothing in the suite talks to `teamtavern.net`.

## Smoke checks

1. `/api/games/<handle>` with a `HeadlessChrome` user agent returns JSON.
2. `/games/<handle>` with a Googlebot user agent returns HTML containing at least one listing and a game-specific `<title>`.
3. `/games/<handle>` in a real browser renders the listings.
4. `/api/games/<handle>` with a browser user agent returns JSON with the game's fields.

## Out of scope

User flows (register, post, search). They are written against the redesigned pages, not the current ones.
