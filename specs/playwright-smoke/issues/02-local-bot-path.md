# Make the bot path work locally

Status: ready-for-agent

The bot handler rewrites to `rendertron:3000/render/{scheme}://{host}{uri}`. Locally the host is `localhost`, which inside the rendertron container is rendertron itself, so the prerender returns 400 and the smoke check cannot exercise the real path.

## To do

- Give the rendertron container a name for the site it can resolve: serve the site under a second name in the development and test Caddyfiles, for example `http://caddy`, so rendertron can fetch it over plain HTTP on the compose network without a certificate.
- Make the render origin configurable instead of copying the incoming scheme and host: an environment variable read by the Caddyfile, set to `https://www.teamtavern.net` in production and to the compose-internal name locally.
- Keep production behaviour identical: the production Caddyfile still renders `https://www.teamtavern.net{uri}`.
- Remove the "Rendertron returns 400" entry from the expected-noise list in `CLAUDE.md`, since it no longer happens.

## Done when

`curl -k -A Googlebot https://localhost:8443/games/apex` returns HTML with listings in it. The test stack seeds `apex`, `hots`, `r6s` and `splitgate`; it has no `valorant`.
