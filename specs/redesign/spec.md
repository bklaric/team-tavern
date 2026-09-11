# Redesign

TeamTavern rebuilt around posts instead of profiles, games that cost one catalogue entry to add, and pages that crawlers can read. Vocabulary is in `CONTEXT.md`; the two structural decisions are `docs/adr/0001-no-team-entity.md` and `docs/adr/0002-server-render-crawlable-pages.md`. The numbers behind the decisions come from the production dump of 2026-09-11, restored into the local database.

## Why

- Adding a game means researching its modes and ranks and producing sixteen assets. Eleven games exist; four are dead.
- Search traffic collapsed in May 2025 because bots were served empty pages (`specs/prerender-fix`). Game pages still earn a fifth of clicks at a 7% click-through rate for "X team finder" queries, so the crawlable surface matters.
- Player profiles, team entities and team profiles duplicate every detail with different arity, and the two-step team flow loses 29% of teams before they post.

## The model

- **Player**: one sign-in identity (email and password, or Discord), one editable non-unique contact email, contacts per platform, languages, location, timezone, birthday. Nothing is linked between identities; two identities are two players.
- **Game**: handle, title, short title, IGDB id and cached cover. Optional rank, role and mode option lists, at most one of each, added only when the game earns them. A rebrand renames in place; a shut-down game is deleted with its posts; a sequel is a new game.
- **Post**: one player, one game, one direction (looking for a team, looking for players). Platform, availability, microphone, new or returning, one about text, rank, role and mode values. Rank is a single value when looking for a team and a set when looking for players. A "looking for players" post also carries group size (party or community), an optional group name, an optional Discord invite, and wanted age range, wanted locations and wanted languages. At most one post per player per game per direction.
- **Age tiers**: fresh up to 30 days since renewal, stale to 90 days and visibly marked, archived after that and hidden from search but visible and renewable to its owner. Editing renews. Emails at 30 and 90 days with one-click renew.
- **Missing availability** matches every availability filter and shows as not specified.

## Pages

- Home: the game grid, a search box, one "Post" call to action.
- `/games/<handle>/players` and `/games/<handle>/teams`: the listings for each direction, filters, sorted by recency with the age shown. The game URL opens on players looking for a team. Titles, headings and descriptions come from a template on the game title.
- `/players/<nickname>`: the player's posts. There is no dedicated post page.
- Post creation starts before registration, as preboarding does now.
- Home and the listing pages are rendered to HTML by the Node server for bot user agents. Rendertron is removed.

## Migration

One transaction at cutover. Player profiles become "looking for a team" posts; team profiles become "looking for players" posts owned by the team's owner, with the team's name and Discord invite copied over. About and ambitions are joined into one text. Renewal date is the profile's last update, so most migrated posts start archived. Secondary rank fields and their values are dropped; rank, role and mode fields are kept per game. All players are kept, including the 15731 pre-2023 nickname-only accounts. Alerts are dropped.

## Ads

Placements stay except the video slider. The mobile takeover is kept but not shown on the first page view of a session. Mobile Core Web Vitals, first-page exit rate, post-creation rate and takeover revenue are recorded before cutover for comparison.

## Follow-ups, not in scope

Saved searches replacing alerts; user-requested games with approval; Steam and Google sign-in; retiring CS:GO for CS2 and reconciling Overwatch and Splitgate; an admin route for games and field lists; serving server HTML to browsers.
