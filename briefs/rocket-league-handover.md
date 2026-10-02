# Rocket League: handover for choosing a guide topic

Written 2026-10-02, at the end of the session that added Rocket League to the
catalogue. The next session picks a Rocket League guide topic and writes its
brief, as `marvel-rivals-championship-team-brief.md` did for Marvel Rivals. Read
the `blog-plans` memory first: game guides are spokes of the join guide, one per
game's own team format, and they target what a feed can't answer. "Rocket League
team finder", "LFG", "duo finder" and the like belong to `/games/rocket-league`.

## State of the work

Nothing is committed. The working tree on master holds:

- `src/TeamTavern/Database/Seed/Games/RocketLeague.sql`, the seed
- `Client/Static/Images/Games/rocket-league.webp`, Steam's 600x900 cover
- a new contact kind, `epic` ("Epic ID", column `player.epic_id`), with
  `Migrations/2026-10-02-epic-id.sql`. That migration runs after
  `2026-09-29-marvel-rivals-username.sql`, and both have to reach the
  development and production databases before the release that reads the
  column.
- specs updated from 11 games to 12, and a contact-panel test for the Epic ID

Verified: `spago build`, `verify-seed.sh` on the file and with `--all`, the
migration's schema dump against `TablesCurrent.sql`, `npm run typecheck`, and
games, home, header, smoke, crawlers, client-errors, contact, feed and phone
specs. The feed and card were checked by eye at 375 and 1280 px.

One decision is still open with the user: whether Rocket League keeps its
platform field on the card (see Crossplay below).

## What the seed says

| Field | On card | Options |
| --- | --- | --- |
| 2s rank, 3s rank | yes | Bronze I … Grand Champion III, Supersonic Legend (22) |
| 1s, Hoops, Rumble, Dropshot, Snow Day, Heatseeker rank | Details | the same 22 |
| Server | Details | the game's 12, spelled as its menu spells them |
| Platform | yes | PC, PlayStation, Xbox, Switch |
| Looking for | yes | Casual, Ranked, Scrims and tournaments, Learning the game, RLCS |

Contacts: Discord, Epic ID, PSN ID, Xbox gamertag, Nintendo friend code.
Trackers: rocketleague.tracker.network for Epic, PSN and Xbox. There is no role
field: posts ask for someone who rotates, never for a position.

## The game as of Season 24

Every rocketleague.com and epicgames.com page returns 403 to WebFetch. They load
in the playwright-chrome browser.

- **Season 24** started 2026-09-23 with patch v2.76
  ([patch notes](https://www.rocketleague.com/news/rocket-league-s24-patch-notes-v276)).
  No end date is published; the season-long "Run it Up" event ends 2026-12-09.
  An Unreal Engine 6 "new era" was teased at the Paris Major on 2026-05-24,
  with no date ([Gematsu](https://www.gematsu.com/2026/05/unreal-engine-6-announced-with-rocket-league-reveal)).
- **Platforms:** Epic, Steam (unlisted for new players, still patched for
  owners), PlayStation, Xbox, Switch and Switch 2. Mac and Linux online play
  ended in 2020. Free to play since 2020.
- **Crossplay:** one pool for every platform, ranked included, on by default.
  A player can switch it off, but can't exclude only one platform. Rank is one
  per player across platforms, taken from the Primary Platform linked to the Epic
  account ([Psyonix, Aug 2020](https://www.rocketleague.com/news/cross-platform-progression-with-free-to-play--a-closer-look)).
  So unlike Overwatch and Marvel Rivals, PC and console players climb the same
  ladder together.
- **Ranks:** Bronze I to Grand Champion III, then Supersonic Legend, each split
  into Divisions I–IV. The game writes Roman numerals; posts write "C2",
  "Diamond 3", "GC1".
- **Ranked playlists:** Duel 1v1, Doubles 2v2, Standard 3v3, plus Hoops,
  Rumble, Dropshot, Snow Day and Heatseeker (2v2). Quads 4v4 was ranked in
  Seasons 20–21 and is casual only now. Patch notes for Seasons 20, 21 and 22
  record the changes.
- **Tournament rank** is a separate ladder from the competitive ranks
  ([Epic help](https://www.epicgames.com/help/rocket-league-c-37599050/gameplay-c-32343914/why-is-my-tournament-rank-different-from-my-competitive-rank-on-rocket-league-a12640184)).
  Its tiers weren't checked.
- **Servers**, as the in-game Regions menu shows them: US-East, US-Central,
  US-West, Europe, Asia-SE Mainland, Asia-SE Maritime, Asia-East, Middle-East,
  Oceania, South Africa, South America, India. The menu was read from a frame at
  0:58 of [JustBOZ's guide](https://www.youtube.com/watch?v=trj9fne3jVk); the
  last two were scrolled out of view and come from Epic's help page. The
  Fandom wiki's spellings ("Asia SE-Mainland", "Asia East") are wrong.
- **Identity:** the friend list is Epic's on every platform, and posts trade
  Epic IDs ("Epic ID" 11 posts against "Epic name" 5).

## Team formats a guide could be about

### In-game tournaments

From [Epic's help page](https://www.epicgames.com/help/rocket-league-c-37599050/gameplay-c-32343914/what-are-competitive-auto-tournaments-in-rocket-league-a12090278):

- 32-team single-elimination brackets, best-of-three semifinals and finals,
  and a Second Chance bracket
- registration as a full team, an incomplete team or one player
- a mixed-rank party is placed by its highest player
- the tournament region is set in Settings and can change once every 24 hours

Season 22 added Weekly Cash Cups through Repeat.gg, earned by playing ranked.

Unconfirmed: which formats run (2v2 and 3v3 come from a search snippet only),
the schedule per region, and how often. Epic's page reads "a scheduled
tournament each week", while posts speak of "daily tournaments" and "the 10 PM
tournament tonight". Settle this before anything is written.

### RLCS

[RLCS 2026](https://www.rocketleague.com/news/rocket-league-championship-series-2026-season)
(announced 2025-09-15):

- 3v3, in two splits, each with three online Opens per region
- two Majors, the second in Paris on 20–24 May
- 1v1 and 2v2 Opens in June and July 2026
- seven regions: NA, EU, SAM, APAC, MENA, OCE, SSA
- at least two of a roster's three players must be citizens or permanent
  residents of the region
- open to anyone; one post says registration is on start.gg

The 2026 World Championship crowned 3s, 2s and 1s in September.

[RLCS 2027](https://www.rocketleague.com/news/rlcs-returns-to-london-this-december-for-the-club-championship-with-dollar25m-up-for-grabs)
(announced 2026-09-20) is a Club Championship:

- 20 invited clubs and 4 places from an Open Online Qualifier starting in
  November
- a London kickoff on 9–13 December, then regional online league play and a
  16-team LAN, with $2.5M in prizes

**This is the most timely hook:** a qualifier anyone can enter, opening within
weeks, under a new format nobody has explained yet.

Not checked: the qualifier's dates, its registration route and its roster
rules. "Club" there probably means an esports organisation, not an in-game
Club, so confirm that before writing.

### In-game Clubs

Up to 20 members, with owner and manager roles, club stats, and Medals and Club
Titles each season
([Epic help](https://www.epicgames.com/help/rocket-league-c-37599050/gameplay-c-32343914/clubs-in-rocket-league-a10103119)).
There is no club browser in the game, which is a gap a guide or a community post
could fill. Posts do look for clubs, and some only want one for the club
trophies.

## What players post

The sample is the 600 newest posts on r/RocketLeagueFriends, from 2026-05-11 to
2026-10-01. 267 of them are from the last two months, and those are the counts
below unless marked otherwise. The other LFG subreddits tried (RLFriends,
RocketLeagueLFG, RocketLeagueTeams, RLCSTeams) are empty or dead. TeamTavern
never had Rocket League, so its database has nothing.

| Topic | Posts (last 2 months) |
| --- | --- |
| Ranked | 78 |
| Casual | 47 |
| Learning, coaching, returning | 43 |
| Help with event challenges (Kingdom Hearts collab) | 29 |
| Tournaments | 14 |
| Clubs | 12 |
| RLCS | 6 |
| Scrims | 6 |
| Orgs or rosters | 5 |
| 2s | 88 |
| 3s | 43 |
| 1s | 14 |
| Extra modes | about 18 (36 of all 600) |
| A server, mostly NA East or West | about 19% of all 600 |

Of all 600 posts, 44 mention a division and 55 mention Epic.

What stood out in reading them:

- **Tournament posts.**
  - Some recruit a teammate for a particular in-game bracket ("teammate for
    2v2 tournament, diamond and above, tomorrow 9pm"; "looking for a teammate
    for the 10 PM tournament tonight").
  - Some advertise community cups and leagues: P2I weekly, 8Bit Open, NARL,
    86 Leagues.
  - A few look for a roster for RLCS ("GC3 and GC2 looking for a 3rd for comp
    3s and future RLCS quals"; "RLCS June 2026, need 2 more over Champ 2").
- **A recurring complaint:** "solo q teammates" and a long-term duo to climb 2s
  with, often with an age bracket (21+, 30s).
- **Challenge help:** a large share of posts ask for help with the current
  event's challenges.

The raw posts and the counting scripts are outside the repo, in
`C:\Users\BranimirKlaric\rl-research\`:

- `RocketLeagueFriends.json` and `RocketLeague.json`, the posts
- `count.cjs`, which counts terms and prints matching posts:
  `count.cjs <posts.json> <since> [label]`
- `peek.cjs`, which prints a sample
- `play.cjs`, which captures YouTube frames (below)

Its `league` regex matches "Rocket League" itself, so ignore that row.

## How to get past blocked sources

- **rocketleague.com, epicgames.com and Fandom** load in the playwright-chrome
  browser. Read a page's text with `browser_evaluate` returning
  `document.body.innerText`. The MCP writes snapshots to `.playwright-mcp/` in
  the repo root, which isn't git-ignored, so delete it afterwards.
- **Reddit:** `.claude/skills/game-seed/scripts/reddit-posts.cjs`, run with
  `"$(volta which node)"`.
- **What the game shows on screen:** `rl-research/play.cjs <videoId> <prefix>
  <start> <every> <count>`. It opens the watch page headless, clicks the
  Croatian consent button ("Prihvati sve"), waits out or skips the ads,
  re-seeks if an ad swallowed the seek, and saves a frame every few seconds.
  Embeds fail with error 153, and seeking far past what is buffered leaves a
  spinner, so it seeks near the target and lets the video play.
- **Search Console:** the query script recipe is in the `google-api-access`
  memory. Nobody has pulled Rocket League queries yet; that is the first step
  for choosing the topic.

## Suggested next steps

1. Pull Search Console queries containing "rocket league", "rlcs" or "rl"
   (tournament, club, team, 2s, 3s) for Jun 2025 to now. The site never had the
   game, so expect little, as with Marvel Rivals.
2. Look at the current results for "how to join RLCS", "RLCS open qualifier
   2027", "rocket league club championship qualifier", "rocket league
   tournaments how do they work" and "rocket league club how to join".
3. Settle the unconfirmed facts above from primary sources: the tournament
   schedule and formats, the 2027 qualifier's dates, registration and roster
   rules, and the tournament rank's tiers.
4. Choose between an RLCS 2027 qualifier guide, which is timely, and an in-game
   tournaments guide, which is evergreen with more posts behind it, and write the
   brief to the Marvel Rivals brief's shape.
