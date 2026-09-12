---
status: accepted
---

# Crawlable pages are rendered to HTML by the Node server

Every page the sitemap lists, home, the games list, and per game the landing page and the two listings, is rendered on demand by the Node server, from the same queries the API runs, using the string renderer for halogen-vdom. Caddy sends bot user agents to that server; browsers get the SPA. There is no headless-browser prerenderer. The previous prerenderer served every data-driven page empty for sixteen months because its browser's API calls were caught by a Caddy rule, and nothing could notice: a rendering layer that runs a browser cannot be checked without running one. Rendering in the server process makes the crawlable HTML a plain function of data that a test can call.

## Considered options

- Keep a prerenderer such as rendertron or a Playwright-based service. Rejected: rendertron is archived upstream, and any browser-based layer keeps the failure mode of silently rendering nothing.
- Rely on Google rendering the SPA itself. Rejected: rendering is queued behind crawling, and preview bots for Discord, Slack and Twitter render nothing at all.
- Hosted dynamic rendering. Rejected: a recurring cost for the same blind spot.

## Consequences

Views for the crawlable pages must be plain Halogen HTML with no child components, shared by the SPA and the server. Halogen does not hydrate, so if the server HTML is ever served to browsers the SPA replaces the DOM once it loads.
