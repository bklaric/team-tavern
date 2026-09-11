# Client pages

Status: ready-for-agent
Blocked by: 03, 04

The SPA rebuilt around the post model. Old profile, team, game landing and alert pages go.

## To do

- **Home**: the game grid with covers, a search box that filters the grid, one "Post" call to action. The per-game landing page and its marketing sections are removed.
- **Listings**: one page component for `/games/<handle>/players` and `/games/<handle>/teams` with a direction toggle that changes the URL, the filters from the posts API, results sorted by recency with the age tier shown, and "not specified" where availability is missing. The game URL redirects to the players page.
- **Post creation**: starts from a listing page or the home call to action, collects the post before asking for a sign-in identity, and finishes by creating both. Direction is the first choice.
- **Player page** at `/players/<nickname>`: the player's posts in both directions, with edit, renew and delete for the owner. Archived posts appear only to the owner, marked as such.
- **Settings**: contact email, contacts per platform, languages, location, timezone, birthday.
- Router cases, `Client/Style/Main.scss` registrations and the sitemap updated; the deleted pages' stylesheets removed.

## Done when

A visitor can find posts for a game, create a post and account in one flow, and renew it from their player page, in the running stack.
