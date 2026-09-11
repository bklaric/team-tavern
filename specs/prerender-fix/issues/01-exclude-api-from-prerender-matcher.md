# Exclude the API from the prerender matcher

Status: resolved

The change is in the working tree: `@prerender_header` in `stacks/base.Caddyfile` gains `not path /api/*`, with a comment explaining Caddy's handle ordering. Verified locally by adapting the file with `caddy:2.10.0` and by reloading the local Caddy: a `HeadlessChrome` user agent gets JSON from `/api/games/valorant` and the prerender HTML from `/games/valorant`; a browser user agent gets `index.html`.

## To do

1. Commit the Caddyfile change.
2. On the server, pull and reload Caddy without a restart:

   ```bash
   docker exec caddy caddy reload --config /etc/caddy/Caddyfile
   ```

3. Confirm from outside:

   ```bash
   curl -s -A "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/120.0.0.0 Safari/537.36" https://www.teamtavern.net/api/games/valorant | head -c 80
   ```

   JSON rather than HTML means the prerenderer's browser reaches the API.

## Done when

A `HeadlessChrome` request for `/api/games/valorant` returns JSON. What the crawler then sees on the page is the spec's outcome and needs `03-initialize-the-ad-queue.md` as well.

## Comments

Live on production. The check above returns JSON, and the rendertron image's own Chrome records 200 for both `/api/games` and `/api/games/valorant` while loading the game page.

The page still prerenders empty, because the ad queue throws before the app can use that JSON. That is the second fault, tracked in `03-initialize-the-ad-queue.md`.
