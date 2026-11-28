# Dota 2 ranked: handover for writing the brief

Written 2026-11-28, at the end of the session that wrote
`apex-legends-ranked-with-friends-brief.md`. The next session writes
`briefs/dota-2-ranked-with-friends-brief.md` the same way, in a cloud session,
and stops at the brief: the user reviews it before any guide is written.

## State of the work

Both files are on branch `ccr-f2206c87-q5t7nx`, not on master: the Apex brief
(commit 9d0706d) and this handover. Start the new session on that branch, or
fetch it, or the model brief below is missing.

Nothing about Dota ranked has been researched yet beyond the leads under
"Leads already found". The user chose this topic from these suggestions on
2026-11-28. The reasons, from the planning folder's `SUBREDDITS.md`
(9 October 2026) and the Battle Cup brief:

- Dota 2 had 25 active posts on 9 October, the third-busiest feed after
  Valorant (33) and Marvel Rivals (30), and its only guide is the Battle Cup.
- Dota is the game the site's Reddit posts did best in: r/DotA2 posts scored
  390, 330, 171, 131 and 122 between 2019 and 2023.
- 38% of Dota 2 player posts and 53% of group posts tick "Battle Cup"
  (Battle Cup brief), so a ranked guide reaches the rest.

## Read first

1. `.claude/skills/guide/SKILL.md`: the workflow, the shape and the house
   rules.
2. `briefs/apex-legends-ranked-with-friends-brief.md`: the model for this
   brief. Same sections, including "Not checked in this session", which the
   user asked for in every cloud-session brief: everything the session
   couldn't verify, grouped as evidence the workflow asks for, facts to
   confirm, and quotes.
3. `briefs/counter-strike-2-ranked-with-friends-brief.md`: the model for a
   game whose publisher doesn't publish the party rule. Valve doesn't for
   Counter-Strike either, so that brief sources the rule from the client's
   own strings and patch notes, and gives players' reports as reports.
4. `briefs/dota-2-battle-cup-brief.md`: Dota facts already sourced (Dota
   Plus, tickets, calibration, the client's Battle Cup text) and its line
   "Dota's party MMR gap stays out: Valve publishes none". This brief is where
   that question gets answered, as far as the client allows.
5. `src/TeamTavern/Database/Seed/Games/Dota2.sql`, for the field labels the
   guide's "Post on TeamTavern" paragraph names.
6. `src/TeamTavern/Server/Guide/join-an-esports-team.md`, the Dota 2 H3,
   which the guide's spoke sentence goes into.

## What the seed says

| Field | On card | Options |
| --- | --- | --- |
| Rank | yes | Herald 1 … Divine 5, then Immortal (36) |
| Position (slotted) | yes | Carry (1), Mid (2), Offlane (3), Soft support (4), Hard support (5) |
| Server | no | US West, US East, South America, Peru, Chile, Argentina, Europe West, Europe East, Russia, Dubai, South Africa, India, SE Asia, Japan, Australia, China |
| Battle Cup tier | no | Tier 3 … Tier 8 |
| Looking for | yes | Casual, Ranked, Scrims and tournaments, Learning the game, Battle Cup |

Contacts: Discord and Steam. Trackers: OpenDota and Dotabuff, by Steam. PC
only, so no platform field, and no in-game leader field.

## Leads already found

Valve's client text, from SteamDB's GameTracking-Dota2
(`game/dota/pak01_dir/resource/localization/dota_english.txt`), fetched
2026-11-28. These are leads: the next session reads them in context, checks
which are still used, and dates them from the repo's history.

- `CompetitiveMMSpreadTooLarge`: "Party members' MMRs are too far apart to
  participate in ranked matchmaking."
- `CompetitiveMMLargeRankSpreadWarning`: "Your party has a larger than
  recommended difference in ranks between party members. This will result in
  the matchmaker providing you lower quality matches."
- `CompetitiveMMRankPreventsParties`: "Party members' MMR is too high to play
  ranked modes as a party."
- `CompetitiveMMNotUnlocked_Party`: "all party members must have played at
  least {d:required_play_time} hours of unranked matchmade games." The number
  is a parameter; find it elsewhere.
- `CompetitiveMMNoLowPriority_Party`, and `DOTA_MatchmakingRestrictedFromRanked_Description`
  ("A member of your party has a low Behavior Score, preventing you from
  queueing for Ranked Games").
- `DOTA_Chat_AbandonedRankedBeforeFirstBloodParty`: "When a player abandons a
  ranked game before first blood, their entire party receives a loss".
- "Party MMR" strings (`dota_general_mmr`, `DOTA_Profile_Party_MMR`) and team
  MMR for a party of five playing under a team identity. Find out whether
  party MMR still exists or is legacy text.
- `CompetitiveMMRTooHighS2` mentions the Reborn beta: old strings stay in the
  file, so not every string is live.

## Questions the brief has to answer

- Party sizes in Ranked: one to five, or is four refused, as in some games?
  Ranked roles and how a party picks roles.
- How far apart a party can be: the "too far apart" refusal and the "larger
  than recommended" warning. Which rank spread triggers each? Valve likely
  publishes no number; find patch notes that changed it, and treat players'
  reports as reports.
- Who can't party at all: which ranks the "too high to play ranked modes as a
  party" message covers (Immortal, or a rank threshold), and since when.
- What every party member needs: the unranked hours, phone number or
  behaviour score requirements, and low priority.
- How a party is matched and what a party's win or loss does to each player.
- What happens when one player abandons, for the whole party.
- Turbo, unranked and Battle Cup as places a mixed-rank group can play
  instead, in one or two sentences, linking the Battle Cup guide.
- What the top results and AI Overview say, and where they're wrong.

## What worked in this session

- **Google autocomplete:** `curl -G https://suggestqueries.google.com/complete/search --data-urlencode client=firefox --data-urlencode hl=en --data-urlencode gl=us --data-urlencode q=<seed>`
  over a list of seeds. The planning folder's suggest scripts aren't in a
  cloud session.
- **Valve's client text:** `raw.githubusercontent.com/SteamDatabase/GameTracking-Dota2/master/...`
  answers from the container. The repo's commit history, for dating a string,
  hasn't been tried: GitHub's commits page for the file, or the git history,
  may need trying.
- **Valve's announcements:** the Steam news API answers from the container:
  `https://api.steampowered.com/ISteamNews/GetNewsForApp/v2/?appid=570&count=…`.
  The CS2 brief searched every announcement this way for "party".
- **Publishers' pages:** curl with a browser user agent got pages that
  WebFetch couldn't (help.ea.com gave WebFetch 503s). Chromium through this
  environment's proxy got 502s from ea.com, though curl got the same pages.
  Try curl first, then Chromium (`require('playwright')`, no install needed).
- **web-fetch agents** summarise and cap quotes at 125 characters on most
  sites. Use them to find what a page says, then fetch the full text with curl
  and check every quote word for word before it goes in the brief.
- **Bot walls:** forums.ea.com blocked both WebFetch and Chromium. Valve's own
  sites weren't tried. A page behind a wall goes into "Not checked in this
  session" by name.

## What a cloud session can't do

List each of these in the brief's "Not checked in this session":

- **GUIDES.md** isn't in the repo or in Drive. Drive's copy of the planning
  folder is from 9 October and has no GUIDES.md, so there's no Search Console
  evidence for Dota ranked.
- **The production backup** isn't available, and Docker isn't running, so
  there are no post counts. Name the counts the guide would want, as the Apex
  brief does.
- **Google's results page and AI Overview** weren't loaded in the Apex
  session; a search API's results stood in. Try loading Google in Chromium
  once, and if it walls, say so.
- **Anything that needs the game client:** how a refusal actually reads,
  where the line falls. The user checks these on their computer.

## The user's standing choices

- The user approves every commit. For the Apex brief they said to commit and
  push once the "Not checked" section was in; ask again for this one unless
  they say the same.
- No Reddit or community Discord named or linked in the guide; players'
  reports are read and never cited (skill house rules).
- Present tense, British spelling, sources linked on the words that name
  them (skill house rules).

## Suggested next steps

1. Read the files under "Read first".
2. Autocomplete for Dota ranked seeds: "dota 2 party", "dota 2 rank
   difference", "dota 2 party mmr", "dota 2 ranked with friends", "dota 2
   immortal party", "dota 2 ranked requirements", "dota 2 ranked hours",
   "dota 2 role queue party", "can you play ranked dota 2 with".
3. The client text: read the strings above in context and date them.
4. Valve's announcements since 2019 that mention party, ranked or MMR, by the
   Steam news API, and the patch notes they link.
5. The top results and what they claim.
6. Write the brief in the Apex brief's shape, with "Not checked in this
   session" complete, and show it to the user.
