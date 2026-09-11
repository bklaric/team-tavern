# Server-rendered pages

Status: ready-for-agent
Blocked by: 03

Home and the two listing pages per game rendered to HTML in the Node server for bot user agents, per `docs/adr/0002-server-render-crawlable-pages.md`. Rendertron goes.

## To do

- Add `halogen-vdom-string-renderer` to `spago.yaml`. Write the home and listing views as functions from data to slot-free Halogen HTML in `Client/Pages/`, used by both the SPA components and the server.
- A `Server/Render/` handler for `/`, `/games`, `/games/<handle>/players` and `/games/<handle>/teams` that runs the same queries as the API, renders the view into `index.prerender.html`'s shell with the templated title, description and canonical link, and returns it. Unknown handles return 404.
- `stacks/base.Caddyfile`: the bot user agent block proxies to `node:8080` under a render prefix instead of rendertron; the `@prerender_header` block and the rendertron service in `docker-compose.yml` are removed.
- The smoke suite's bot check (`specs/playwright-smoke`) is updated to the new path and still passes.
- `CLAUDE.md`: the request-flow and running-the-stack sections describe the new bot path; the rendertron noise entry is removed.

## Done when

A Googlebot request for a game listing page returns HTML with the listings, the game-specific title and no script tags for ads or analytics, and the rendertron container no longer exists.
