# The Finals: handover for choosing a guide topic

Written 2026-10-05, at the end of the session that added The Finals to the
catalogue. The next session investigates a guide for it and writes its brief,
as `marvel-rivals-championship-team-brief.md` and
`rocket-league-tournaments-brief.md` did. Read the `blog-plans` memory first:
game guides are spokes of the join guide, one per game's own team format, and
they target what a feed can't answer. "The Finals team finder", "LFG" and the
like belong to `/games/the-finals`.

## State of the work

Committed on branch `the-finals` as f5aa5923, off master, unmerged and
undeployed. The game launches only once the Rocket League promotion is done.
The commit holds:

- `src/TeamTavern/Database/Seed/Games/TheFinals.sql`, the seed
- `Client/Static/Images/Games/the-finals.webp`, a SteamGridDB grid
  (`3234ea69d7084f8bd79a5c228d61c782`) the user chose, with the logo at the top
- a new contact kind, `embark` ("Embark ID", placeholder `Name#1234`, column
  `player.embark_id`), with `Migrations/2026-10-04-embark-id.sql`. It runs after
  `2026-10-02-epic-id.sql`, and has to reach the development and production
  databases before the release that reads the column.
- specs updated from 12 games to 13, and a contact-panel test for the Embark ID

Verified: `spago build`, `npm run typecheck`, `verify-seed.sh` on the file and
with `--all`, the migrations' schema dump against `TablesCurrent.sql`, and the
contact, games, home, header, smoke, crawlers and client-errors specs. The feed
was checked in the test stack. The cover was replaced after the specs ran; it is
600x900, as the build requires.

## What the seed says

| Field | On card | Options |
| --- | --- | --- |
| Rank | yes | Bronze 4 … Diamond 1, then Ruby (21) |
| Class (slotted) | yes | Light, Medium, Heavy |
| Platform | yes | PC, PlayStation, Xbox |
| Looking for | yes | Casual, Ranked, Scrims and tournaments, Learning the game |

Contacts: Discord, Embark ID, PSN ID, Xbox gamertag. Tracker:
thefinals.arenyze.com, by Embark ID. There is no in-game leader field and no
server field. The user decided Class stays slotted although Heavy, Heavy,
Medium is the ranked meta, and the Grand Major stays inside Scrims and
tournaments rather than becoming an option of its own.

## The game as of Season 11

- **Seasons:** Season 11 started 2026-07-09
  ([MP1st, patch 1.000.147](https://mp1st.com/title-updates-and-patches/the-finals-season-11-update-fires-out-on-july-9-via-patch-1-000-147)).
  Season 12 comes in October. Its date is unconfirmed: one YouTube video says
  October 20, while arenyze showed Season 11 ending about October 10. A guide
  written now describes Season 11 and may need checking against Season 12's
  patch notes.
- **Platforms:** PC (Steam), PS5, Xbox Series X|S. The PS4 version shut down on
  2026-03-18 ([Wikipedia](https://en.wikipedia.org/wiki/The_Finals)). Crossplay
  covers all three and is on by default, and a player can turn it off
  ([Embark help](https://id.embark.games/the-finals/support/faq/57-cross-play-on-all-platforms)).
- **Identity:** the Embark ID, `Name#1234`, holds friends across platforms
  ([Embark help](https://id.embark.games/the-finals/support/faq/245-what-is-embark-id)).
- **Ranked** is Ranked Cashout. Its leagues are Bronze, Silver, Gold, Platinum
  and Diamond, each with divisions 4 to 1, 2,500 Rank Score apart, from 0 for
  Bronze 4 to 47,500 for Diamond 1. Ruby goes to the top 500 at the season's
  end, and the cutoff was 55,489 RS on 2026-10-04 (finalstracker.net). New
  players play four placement tournaments
  ([thefinals.wiki](https://www.thefinals.wiki/wiki/Ranked)). A party is matched
  by its highest-ranked player
  ([Embark help](https://id.embark.games/the-finals/support/faq/80-ranked-cashout-1772203056)).
  Season 11 added performance bonuses to Rank Score.
- **Unconfirmed:** the format of a Ranked Cashout tournament. Older sources say
  16 teams of three over four rounds, and a 2026 one says eight teams, with the
  final round played under Head2Head rules. Settle it before writing anything
  about ranked.
- **World Tour** is a progression across every mode, with badge leagues from
  Bronze to Emerald, and a loss never costs points. It isn't a skill ladder
  ([thefinals.wiki](https://www.thefinals.wiki/wiki/World_Tour)).
- **Modes:** Cashout, Ranked Cashout, Quick Cash, Point Break, Power Shift,
  Team Deathmatch, Head2Head, Final Round, and Cashout with bots. Cashout is
  played as a "tournament" in the game's own words, ranked or not.
- **Servers:** players pick a region in settings. The official Discord's LFG
  channels are EU, NA, Asia, OCE and South America, and OCE players ask whether
  to play on NA or Asia servers when OCE queues are empty.

## The competitive scene

From [Embark's Road to the Grand Major 2026](https://www.reachthefinals.com/patchnotes/tgm26-lcoq),
[What's happening in 2026](https://www.reachthefinals.com/patchnotes/tgm26-1)
and Liquipedia:

- **The Grand Major** is Embark's yearly event. 2026's is November 27–29 at
  DreamHack Stockholm, with $150,000 and 16 teams: 7 from the Americas, 6 from
  EMEA and 3 from APAC. A team is three players plus an optional sub. Its formats
  are called Promotion and Throne. In 2025 NTMR won it and $37,500.
- **The Online Series** runs four cycles per region, from May to September. Each
  cycle is $4,000, with a Swiss stage of up to 256 teams played in Final Round,
  then Ranked Cashout and Final Round rounds. The cycles drew 68 to 123 teams per
  region.
- **The Regional Fame Index** gives points for the Online Series and for
  community tournaments that meet Embark's standards, and the top teams are
  invited to the Grand Major.
- **The Open Qualifiers** ran in September (APAC 5–6, Americas 12–13, EMEA
  19–20). Anyone could enter, with no rank requirement. They drew 27, 158 and
  142 teams, about 330 teams or 1,000 players. Closed Qualifiers followed on
  September 19, September 26 and October 3.

How prominent it is: 10 of 998 r/thefinals posts in two weeks mention it, nearly
all about watching or "how does this system work" and "is it worth going pro".
None of 1,203 Discord LFG messages name it.

**The timing problem:** the 2026 path is over until the November finals, and
2027's Online Series would start around May. A guide on getting into the
competitive path would be evergreen in shape but would wait months for its
first use.

## Guide candidates

These are open; the next session chooses.

1. **The competitive path:** the Online Series, the Fame Index, the Open
   Qualifiers and community tournaments, for a team of three. r/thefinals asks
   how the system works. The 2026 rules are published; 2027's are not.
2. **Ranked with a party:** how a party is matched by its highest player, Rank
   Score, placements, Ruby, and why solo queue is hard. One r/thefinals post
   blames voice chat being opt-in even to hear; solo-queue complaints run
   through both samples. It risks overlapping the feed's
   job, so it has to answer what the feed can't, such as how ranked treats a
   mixed-rank party.
3. **Clubs:** the game has in-game clubs with tags, and the official Discord has
   a `club-recruitment` forum. They weren't researched.
4. **OCE:** Oceania's ranked queues are thin, and an OCE United community effort
   tried to revive them. It's too narrow for a guide but useful as a detail.

## What players post

**r/thefinalsLFG** has 37 posts since January 2024, and r/thefinals had 6 LFG
posts in its newest 998. These 36 posts are the whole Reddit sample. Ranked
came up 14 times, Quick Cash or casual 15, learning 6, tournaments 3, and a
class 10.

**The official Discord** (guild `1008696016318513243`, invite `thefinals`):
1,203 messages from its ten LFG channels, casual and ranked for EU, NA, Asia, OCE
and SA. They were read in preview mode without joining, which loads about 126
messages per channel. So EU and NA ranked cover only a day or two, and the
quieter regions go back to August. 684 messages have 25 or more characters, and
those are the counts below:

| Topic | Messages |
| --- | --- |
| Ranked | 102 |
| Casual, chill or fun | 92 |
| Cashout | 69 |
| Quick Cash | 26 |
| Other modes | 21 |
| Learning or new players | 28 |
| Asking for a class | 15 |
| Embark ID given | 10 |
| Tournaments, scrims, comp teams, esports | about 1% |
| Grand Major | 0 |
| Shotcaller or IGL | 0 |

Among the 907 distinct messages, 204 name a league, 18 give a league with its
division, and 132 give a Rank Score ("35k"). Authors carry the class roles they
picked on joining: Medium Main on 73 messages, Light Main on 32, Heavy Main on
25.

Larger community servers found in Discover: THE FINALS - Deutsch (16k members),
THE FINALS France (15k), The Finals RU Community (13k), "The Finals LFG,
baybee" (4.7k, "the oldest and largest Finals server"), Ruby Grind (an LFG and
competitive server) and The Finals OCE United (2.2k).

## Raw data and how it was reached

Everything is outside the repo, in `C:\Users\BranimirKlaric\finals-research\`:

- `thefinals.json`, `thefinalsLFG.json`, `TeamRedditTeams.json` and
  `GamerPals.json`, the Reddit posts
- `discord-lfg.json`, the Discord messages by channel
- `count.cjs`, `check.cjs`, `discord-count.cjs`, `discord-rank.cjs` and
  `esports.cjs`, the counting scripts
- `schema-diff.sh`, the migration check
- `cover.mjs`, the cover conversion

How the sources were reached:

- **Reddit:** `.claude/skills/game-seed/scripts/reddit-posts.cjs`.
- **Discord:** the `discord-lfg-preview` memory has the method. Enter a server
  from Discover's search, never by URL, and read only. Joining the official
  server would lift the 126-message limit, but it's the user's call.
- **Liquipedia** returns 403 to WebFetch and loads in the playwright-chrome
  browser.
- **tracker.gg** has no The Finals pages. finalstracker.net profiles drop the
  `#` (`/p/Name-1234`) and cover only the top 10,000. thefinals.gg uses numeric
  ids. arenyze takes the encoded Embark ID and covers unranked players.
- **Search Console:** nobody has pulled The Finals queries yet. The recipe is in
  the `google-api-access` memory.

## Suggested next steps

1. Pull Search Console queries containing "the finals" or "finals" (ranked,
   team, club, esports, major) from June 2025 to now. The site never had the
   game, so expect little.
2. Look at the current results for "how to get into the finals esports",
   "the finals grand major qualifier", "the finals online series",
   "the finals ranked party", "the finals club" and "the finals ruby".
3. Settle the Ranked Cashout format and Season 12's date and changes.
4. Choose a topic from the candidates above, weighing the competitive path's
   long wait against ranked's overlap with the feed, and write the brief to the
   Marvel Rivals brief's shape.
