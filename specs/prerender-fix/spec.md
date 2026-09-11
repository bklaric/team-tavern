# Prerender fix

Bots get their pages from rendertron. Rendertron's headless Chrome loads the page and calls the API with a `HeadlessChrome` user agent. Caddy 2.10 orders `handle` blocks with named matchers before those with path matchers, so the `@prerender_header` block runs before the `/api/*` block and answers those API calls with `index.prerender.html`. The JSON decode fails and every page that loads data prerenders as top bar plus footer under the generic site title.

Evidence: Search Console clicks halve between 2025-05-25 and 2025-06-01 at unchanged average position, two days after the deploy that raised Caddy from 2.5.1 to 2.10.0. Adapting the Caddyfile with both images shows the reordering. A Googlebot request for `/games/valorant` on production returns no content; a `HeadlessChrome` request for `/api/games/valorant` returns HTML.

## Outcome

- Headless Chrome gets JSON from `/api/*` and `index.prerender.html` from page paths.
- A Googlebot request for a game page returns the listings.
- Google is asked to recrawl the game pages.

## Out of scope

Replacing rendertron with server rendering belongs to the redesign. The regression guard is `specs/playwright-smoke`.
