# Make the bot path work locally

Status: resolved

The bot handler rewrites to `rendertron:3000/render/{scheme}://{host}{uri}`. Locally the host is `localhost`, which inside the rendertron container is rendertron itself, so the prerender returns 400 and the smoke check cannot exercise the real path.

## To do

- Give the rendertron container a name for the site it can resolve: serve the site under a second name in the development and test Caddyfiles, for example `http://caddy`, so rendertron can fetch it over plain HTTP on the compose network without a certificate.
- Make the render origin configurable instead of copying the incoming scheme and host: an environment variable read by the Caddyfile, set to `https://www.teamtavern.net` in production and to the compose-internal name locally.
- Keep production behaviour identical: the production Caddyfile still renders `https://www.teamtavern.net{uri}`.
- Remove the "Rendertron returns 400" entry from the expected-noise list in `CLAUDE.md`, since it no longer happens.

## Done when

`curl -k -A Googlebot https://localhost:8443/games/valorant/players` returns HTML with listings in it.

## Comments

The render origin is a Caddyfile **import argument**, not an environment
variable. `base.Caddyfile` rewrites to `/render/{args[0]}{uri}` and each site
file passes its own origin: `http://caddy` in `development.Caddyfile` and
`test.Caddyfile`, `https://www.teamtavern.net` in `production.Caddyfile`. The
environment variable this ticket asked for would have needed `environment:`
wiring on the caddy service in both compose files and a hand edit to
production's `.env`, which lives on the server and is not in the repo; a
variable missing there substitutes an empty origin silently and reintroduces
the 400. The argument keeps every origin in the repo and needs no compose or
env change at all.

Both local Caddyfiles became `localhost, http://caddy { import base.Caddyfile
http://caddy }`. Caddy splits that into a `:443` server matching host
`localhost` and a `:80` server matching host `caddy`, so `localhost` keeps its
certificate and its HTTP redirect while the plaintext name that rendertron can
resolve serves the same site.

`staging.Caddyfile` was a standalone copy of the whole base config and had
drifted: its prerender matcher still lacked the `not path /api/*` exclusion
from `specs/prerender-fix/issues/01`, so the prerenderer's own API calls were
served `index.prerender.html`. It is now
`staging.teamtavern.net { import base.Caddyfile https://staging.teamtavern.net }`,
which fixes that and also gives staging the `/ads.txt` redirect its copy had
commented out. Nothing in the repo selects `staging`, so this is covered by
`caddy adapt` rather than by a boot.

Verified by adapting all four site files against `caddy:2.10.0` — each emits
its own literal origin in the proxy rewrite, staging now carries the `/api/*`
exclusion — and then on a cold test stack. The rendertron log shows
`GET /render/http://caddy/games/valorant 200` where it used to show 400, and
the caddy log has no errors. The browser path and the port-80 redirect for
`localhost` are unchanged.

The "Done when" above named `/games/valorant`, which is the game landing page
and has no listings on it to return. It now names `/games/valorant/players`,
and so do the two listing checks in `spec.md` and issue 04. Both paths were
measured and both go through rendertron: `/games/valorant` prerenders 200 and
14143 bytes with the game-specific `<title>`, against 5.6 KB for the unrendered
shell, and `/games/valorant/players` prerenders the seeded `ValorantTester`
profile and "Showing 1 - 1 out of 1 players".

One consequence for the smoke specs: rendertron injects a `<base href>` when
the document has none, and `Client/Script/Meta.purs` builds canonical and
`og:url` from `window.location.origin`, so local bot HTML carries
`http://caddy` in all of them. That name exists only inside the compose
network. Assertions must read page content, never an asset URL, a canonical
link, or a URL followed out of the bot HTML.
