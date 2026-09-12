# Server-rendered pages

Status: ready-for-agent

Every sitemap page rendered to HTML in the Node server for bot user agents, per `docs/adr/0002-server-render-crawlable-pages.md`. Rendertron goes.

## To do

- Add `halogen-vdom-string-renderer` to `spago.yaml`. Extract the home page, the games list, the game landing page and the two listing pages as functions from data to slot-free Halogen HTML in `Client/Pages/`, used by both the SPA components and the server. Markup and styling stay as they are; only the interactive parts (buttons that open preboarding, filters, ads) remain in the SPA wrapper around the view.
- A `Server/Render/` handler for `/`, `/games`, `/games/<handle>`, `/games/<handle>/players`, `/games/<handle>/teams` and `/about` that runs the same queries as the API, renders the view into `index.prerender.html`'s shell with the page's title, meta description and canonical link, and returns it. Unknown handles return 404. Any other path a bot requests keeps getting the SPA shell from Caddy.
- `stacks/base.Caddyfile`: the bot user agent block proxies to `node:8080` under a render prefix instead of rendertron; the `@prerender_header` block and the rendertron service in `docker-compose.yml` and `docker-compose.test.yml` are removed.
- The smoke suite's bot check (`specs/playwright-smoke`) is updated to the new path and still passes.
- `CLAUDE.md`: the request-flow and running-the-stack sections describe the new bot path; the rendertron noise entry is removed.

## Done when

A Googlebot request for a game listing page returns HTML with the profiles, the game-specific title and no script tags for ads or analytics; a Googlebot request for a landing page returns its content; and the rendertron container no longer exists in either stack.
