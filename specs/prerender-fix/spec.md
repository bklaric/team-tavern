# Prerender fix

Bots get their pages from rendertron. Rendertron's headless Chrome loads the page and calls the API with a `HeadlessChrome` user agent. Two separate faults stop it from ever populating a page, and both have to be fixed for a crawler to see listings.

The first is routing. Caddy 2.10 orders `handle` blocks with named matchers before those with path matchers, so the `@prerender_header` block runs before the `/api/*` block and answers those API calls with `index.prerender.html`. The JSON decode fails and every page that loads data prerenders as top bar plus footer under the generic site title.

The second is the ad queue. `Client/Components/Ads.js` pushes onto `self.__VM`, the queue the Venatus script defines and drains. `index.prerender.html` carries no ad scripts, so `self.__VM` is undefined and every ad slot throws `Cannot read properties of undefined (reading 'push')`. The throw takes down Halogen's queue, so the top bar never resolves its player status and the page component never swaps in the data it fetched.

Evidence: Search Console clicks halve between 2025-05-25 and 2025-06-01 at unchanged average position, days after both the deploy that raised Caddy from 2.5.1 to 2.10.0 and the commit that moved ad insertion into the client. Adapting the Caddyfile with both Caddy images shows the reordering. Driving `dockette/rendertron`'s own Chrome at production reports the `__VM` page error, with both `/api/games` and `/api/games/valorant` answering 200, and serializes the same empty shell rendertron returns to a crawler.

## Outcome

- Headless Chrome gets JSON from `/api/*` and `index.prerender.html` from page paths.
- The prerendered page runs to completion with no page error.
- A Googlebot request for a game page returns the game's posts under a game-specific title.
- Google is asked to recrawl the game pages.

## Out of scope

Replacing rendertron with server rendering belongs to the redesign. Both fixes keep alive the prerenderer that ADR-0002 rejects, and the blind spot that ADR names stays open until server rendering lands. The regression guard is `specs/playwright-smoke`.
