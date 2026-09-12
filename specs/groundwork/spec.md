# Groundwork

Four changes on top of the existing player, team and profile model: one sign-in identity per player, crawlable pages rendered by the Node server, a renewal email for profiles going stale, and the ad changes. Vocabulary is in `CONTEXT.md`; the rendering decision is `docs/adr/0002-server-render-crawlable-pages.md`. The numbers come from the production dump of 2026-09-12, restored into the local database.

## Why

- Search traffic collapsed in May 2025 because bots were served empty pages (`specs/prerender-fix`). Game pages still earn a fifth of clicks at a 7% click-through rate for "X team finder" queries, so the crawlable surface matters, and a headless-browser prerenderer cannot be checked without running a browser.
- A player row can hold an email, a password hash and a Discord id in any combination, so nothing states what signs a player in. 3870 players have only a Discord id and so no address to email; the address Discord already knows is never asked for.
- 96% of profiles are older than a year and nothing tells their owners. Listings sort by last update, so a profile that is still wanted only has to be touched to surface again.
- The mobile takeover fires on the first page view of a session, which is the view a searcher lands on.

## Out of scope

The player, team, profile, game field, filter and alert models stay exactly as they are. The game catalogue, its per-game assets, the landing pages and every other change to what pages look like belong to a later spec that starts from the target UI. `docs/adr/0001-no-team-entity.md` records why the post model was abandoned.

## Sign-in identity

- A player has exactly one identity: a password, used with the nickname or the email, or a Discord account. A check constraint on `player` requires exactly one of `password_hash` and `discord_id`. The dump already satisfies it: no player has both.
- The `email` column is the contact email. It is unique among players with a password, because those may sign in with it, and otherwise not: the unique index on lowercased email is rebuilt with `where password_hash is not null`. Password sign-in and password reset match only rows with a password hash.
- Discord sign-in requests the `email` scope and fills `email` with the verified address when the column is empty. Nothing is ever linked to an existing player: an unknown Discord id registers a new player even if its address is a password player's.
- No rows change, so the schema change is applied by hand as `alter table` statements, per `CLAUDE.md`, and `TablesCurrent.sql` is edited to match.

## Server-rendered pages

Every page the sitemap lists is rendered to HTML by the Node server for bot user agents: home, the games list, and per game the landing page, the players listing and the teams listing. The views become functions from data to slot-free Halogen HTML that the SPA components and the server share; the pages' markup and design do not change. Caddy sends bot user agents to the Node server and the rendertron container goes.

## Renewal email

- A daily job finds profiles whose last update crossed 30 days since the job last considered them and sends one email per player through SendGrid to the contact email, listing every such profile the player owns, directly or through a team, each with a one-click renew link. Players without a contact email are skipped.
- Renewing sets the profile's update time to now, which moves it to the top of its listing, and needs no sign-in. It fires no alerts; alerts fire on creation only.
- A profile is emailed once per crossing: an emailed-at column on both profile tables is set when the email goes out and compared with the update time, so a job that runs twice never sends twice and a renewed profile is emailed again only after another 30 days.
- The rule applies as if it had always existed: a profile 29 days old at deploy is emailed the next day, and one already past 30 days is never emailed for that crossing. At deploy every profile already past the mark has its emailed-at set to its update time.

## Ads

Placements stay except the video slider. The mobile takeover is kept but not shown on the first page view of a session and is never in server-rendered HTML. Mobile Core Web Vitals, first-page exit rate, profile-creation rate and takeover revenue are recorded before cutover for comparison.

## Follow-ups, not in scope

The UI-first redesign, including the catalogue and landing pages; age tiers and archiving of stale profiles; saved searches; user-requested games with approval; Steam and Google sign-in; retiring CS:GO for CS2 and reconciling Overwatch and Splitgate; an admin route for games and field lists; serving server HTML to browsers.
