# Renewal email

Status: ready-for-agent

One email per player when their profiles pass 30 days without an update, each profile with a one-click renew link.

## To do

- `TablesCurrent.sql`: `renewal_emailed timestamptz` and a nullable `renewal_token` on `player_profile` and `team_profile`. Applied by hand at cutover, followed by `update ... set renewal_emailed = updated where updated <= now() - interval '30 days'` on both tables so profiles already past the mark are never emailed for that crossing.
- A daily job in the Node server that selects profiles where `updated <= now() - 30 days` and `renewal_emailed is null or renewal_emailed < updated`, groups them by the player who owns them directly or through a team, skips players without an email, generates a token per profile, and sends one SendGrid email per player listing each profile with its game and a renew link. `renewal_emailed` is set to now for every profile in a sent email.
- An SPA page `/renew/<token>` that calls a new route renewing the matching profile: `updated = now`, token cleared. It then navigates to the profile's player or team page. The route needs no session; an unknown or used token shows a not-found message. Renewing does not fire alerts.
- The 30-day boundary read in `Server/Main.purs` from an environment variable with a default of 30, so a local run can be exercised with a short window.

## Done when

Against the local restore with the boundary overridden, one run emails each qualifying player once with all their crossing profiles, a second run sends nothing, following a link moves the profile to the top of its listing, and the profile is emailed again only after the boundary passes anew.
