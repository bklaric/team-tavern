# Cutover

Status: ready-for-human
Blocked by: 01, 02, 03, 04, 05, 06, 07, 08

One deploy that migrates the database and switches every page. Only the site owner has the server.

## To do

1. Run `backup-database.sh` and confirm the emailed dump restores locally.
2. Rehearse once more: restore that dump locally, apply the migration, run `npm test`, and click through post creation, search and renew in the local stack.
3. On the server: pull, `./build.sh`, apply the migration to the `postgres` container inside its transaction, `./run-stack.sh` so the compose changes (no rendertron) take effect.
4. Verify from outside: a browser loads a listing page with posts; a Googlebot user agent gets the server-rendered HTML with a game-specific title; `/api/games` returns the reduced shape.
5. Search Console: resubmit the sitemap and request indexing for home and the twenty-two listing pages.
6. Record the date in `specs/redesign/ads-baseline.md` for the ads comparison 28 days later.

## Done when

All four checks in step 4 pass on production and the sitemap is resubmitted.
