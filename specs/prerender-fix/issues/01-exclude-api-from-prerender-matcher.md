# Exclude the API from the prerender matcher

Status: ready-for-human

The change is in the working tree: `@prerender_header` in `stacks/base.Caddyfile` gains `not path /api/*`, with a comment explaining Caddy's handle ordering. Verified locally by adapting the file with `caddy:2.10.0` and by reloading the local Caddy: a `HeadlessChrome` user agent gets JSON from `/api/games/valorant` and the prerender HTML from `/games/valorant`; a browser user agent gets `index.html`.

## To do

1. Commit the Caddyfile change.
2. On the server, pull and reload Caddy without a restart:

   ```bash
   docker exec caddy caddy reload --config /etc/caddy/Caddyfile
   ```

3. Confirm from outside:

   ```bash
   curl -s -A "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)" https://www.teamtavern.net/games/valorant | grep -c profile
   ```

   A non-zero count means the page prerenders with listings.

## Done when

The Googlebot request above returns a page with listings and a `<title>` specific to the game.
