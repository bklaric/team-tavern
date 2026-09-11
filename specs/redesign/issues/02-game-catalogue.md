# Game catalogue

Status: ready-for-agent
Blocked by: 01

A game is a seed row with an IGDB id. Everything visual and textual derives from it.

## To do

- A script that, given an IGDB id, fetches the title and cover through IGDB's API with Twitch client credentials from `stacks/.env`, writes the cover into a cached images directory served by Caddy, and prints the seed row.
- Seed rows for the eleven existing games with their IGDB ids and covers; the per-game asset directories under `Client/Static/Images/Landing/` and the game-specific landing video go.
- Rank, role and mode option lists kept only where the current fields have those kinds: rank for every game that has one primary rank, role and mode where they exist. Overwatch keeps one rank.
- Page copy template: title, heading and description for home and both listing pages built from the game title and short title, with an optional per-game override column.
- The `/api/games` and `/api/games/<handle>` routes return the reduced shape: handle, title, short title, cover path, platforms, the three optional field lists, trackers.

## Done when

Adding a twelfth game is one script run and one seed row, and its two listing pages render with templated copy and a cover.
