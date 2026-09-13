# Cutover

Status: ready-for-human
Blocked by: 05, 07, 08

One deploy that applies the schema statements and switches the bot path. Only the site owner has the server.

## To do

1. Run `backup-database.sh` and confirm the emailed dump restores locally.
2. Rehearse: restore that dump into a scratch database, run the check and index predicates from 04 and confirm zero violations, apply the statements from 04 and 07, then run `npm test` and click through sign-in, a Discord sign-in, and a renew link in the local stack.
3. On the server: pull, `./build.sh`, apply the statements from 04 and 07 to the `postgres` container, `./run-stack.sh` so the compose changes (no rendertron) take effect.
4. Verify from outside: a browser loads a listing page with profiles; a Googlebot user agent gets the server-rendered HTML with a game-specific title; a Discord sign-in returns with an email.
5. Search Console: resubmit the sitemap and request indexing for home and the thirty-three game pages.
6. Record the date in `specs/groundwork/ads-baseline.md` for the ads comparison 28 days later.

## Done when

All checks in step 4 pass on production and the sitemap is resubmitted.
