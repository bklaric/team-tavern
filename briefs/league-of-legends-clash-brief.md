# Content Brief: How League of Legends Clash works, and how to find a team for it

The sixteenth guide, and the second for League of Legends after the ranked
one. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md),
and its nearest model is the Dota 2 Battle Cup guide: an in-game bracket for a
five, on a schedule, with tiers and tickets. It covers what a player needs to
know to enter Clash with a five they chose:

- when Clash runs: the 2026 calendar, the week's rhythm, and the time of day
- how a Clash day works: lock-in by tier, scouting, the draft, three games
- tiers: how a team's tier is set, and why one strong player lifts it
- what each of the five needs: level 30, ranked placements (Summoner's Rift
  only), SMS verification on a mobile number, and a ticket
- how to find the rest of the five, in the client and on TeamTavern

League is already on the site, so the guide goes out as soon as it's
written. The next cup, the Summoner's Rift Worlds Clash, opens team formation
on Monday 16 November and plays 21–22 November 2026, so a launch before the
16th catches its searches.

Prepared 2026-11-05. Evidence comes from:

- Google autocomplete for 69 Clash seeds
  (`%LOCALAPPDATA%/Temp/gsc/suggest-clash.mjs`, output `suggest-clash.txt`)
- Riot's [Clash FAQ](https://support.riotgames.com/en-us/league-of-legends/events/clash-faq),
  dated 7 May 2026, and its [SMS Verification page](https://support.riotgames.com/en-us/league-of-legends/events/clash-sms-verification),
  dated 22 May 2026 (copies in `%LOCALAPPDATA%/Temp/clash/`)
- every 2026 patch's notes, 26.1 (7 January) to 26.22 (3 November), grepped
  for Clash (`patches.mjs`, output `patches.txt`)
- [patch 10.18's notes](https://www.leagueoflegends.com/en-us/news/game-updates/patch-10-18-notes/)
  (2020), for free agents
- the community wiki's [Clash page](https://wiki.leagueoflegends.com/en-us/Clash),
  marked Outdated, read through its API
- [wards.lol](https://wards.lol/clash/history/)'s Clash history and schedule,
  which say they come from the Riot API
- web search results for the questions people ask (the SERP and AI Overview
  were not loaded in a browser; do that before writing)
- the local cluster's production copy, `team_tavern`, whose newest post is
  from 26 September 2026

The user chose on 2026-11-05 to give times in Riot's own words, not per
server from the Riot API, so the guide has no per-server timetable and no
`<time>` elements.

## Why this topic

The user chose it on 2026-11-05 from the next-guide check (GUIDES.md, "Topics
not written yet": "LoL Clash. Autocomplete full; schedule queries dominate"):

- **Autocomplete is full**, mostly for dates and times: "lol clash schedule
  2026", "lol clash times", "lol clash times euw", "lol clash time to lock
  in", "clash lock in times", "what time is clash lol today", "when is next
  clash lol 2026". Then rules: "how does clash work lol", "how do clash tiers
  work", "lol clash tier calculator", "lol clash requirements", "lol clash
  ranked requirements", "clash honor level", "clash eligibility",
  "clash unranked lol". Then tickets ("ticket price", "premium ticket vs
  normal", "premium ticket worth it"), rewards ("8 team bracket", "consolation
  bracket rewards", "clash banner levels"), ARAM Clash ("aram clash
  requirements", "aram clash schedule 2026"), and finding a team ("find clash
  teammates lol", "clash find a team greyed out", "lol clash team finder").
- **It's what League posts on TeamTavern are for.** 28% of League player
  posts and 41% of group posts tick "Clash" under "Looking for", and the seed
  has a "Clash tier" field.
- **What ranks is weak.** Booster and smurf-seller pages (turbosmurfs,
  gameboost, 1v9, lolnow, nerfplz) hold the schedule, often an old one, and
  explainers from 2020–2022 hold the rules. Several say a team's tier is the
  average of its players' (Riot: weighted "heavily" toward the best player),
  older ones say Honor level 2 is required (Riot's FAQ: "There is no Honor
  Level restriction"), and some still say Clash runs twice a month or six
  times a year.

## Template

**Recommended**: `how-to-guide`, shaped like `dota-2-battle-cup.md`. The
reader wants to know when the next cup is, whether their five can enter and at
which tier, and how to fill the slots they're short of.

## Target keywords

- **Primary:** lol clash, how does clash work lol
- **Secondary:**
  - lol clash schedule 2026, when is the next clash, lol clash times, lock in
    times
  - lol clash tiers, how are clash tiers determined
  - lol clash requirements, ranked requirements, honor level, unranked
  - lol clash tickets, premium ticket vs basic
  - lol clash rewards, consolation bracket
  - aram clash, aram clash requirements
  - how to join clash lol, find clash team
- **Leave to the feed:** "lol clash team finder", "find clash team", "lfg"
- **Leave out:** drafting advice, team comps and team names; the reward
  tables item by item (link Riot's FAQ for them); Clash's history before 2026;
  Wild Rift and TFT.

## Search intent

Informational with a date in mind. The reader has a five, or part of one, and
wants to know when to be online, whether everyone can enter, which tier
they'll land in, and what a ticket costs them.

## Information gain

- **[UNIQUE INSIGHT] The best player sets the tier.** By Riot's FAQ, team
  tiers "take a weighted average of the roster, but it heavily prioritizes the
  highest-skill players", and "comps of players with vastly different skills
  may have some tougher matches ahead". The client shows a popup for a mixed
  team. Pages that say "average" get this wrong. The advice follows: recruit
  near your own rank, or accept your best player's tier.
- **[UNIQUE INSIGHT] Tier decides when you can lock in.** The lock-in window
  is two hours: Tier IV any time in it, Tier III from half an hour in, Tier II
  from an hour, Tier I only in the last half hour. SEA is different: "all
  tiers can lock in 1 hour before Clash begins." So a Tier I team can turn up
  later, and a Tier IV team can start earlier.
- **[UNIQUE INSIGHT] Plan for three games a day.** The single-elimination
  bracket of eight has a consolation bracket for first-round losers, and
  Riot's reward tables run 0–3, 1–2, 2–1, 3–0. Riot never says every team
  plays three, and its FAQ gives the orb "when you lose or end up winning",
  so the guide states the tables and advises planning for three, not that
  every team plays three. Plan the evening for three games,
  and for five who'll stay for all of them. A team can play Saturday, Sunday
  or both, once each day, with the same five or another.
- **[UNIQUE INSIGHT] What's required, and the conflict over Honor.** Level 30,
  placements "in at least one Summoner's Rift ranked queue this year or the
  year before", SMS verification on a mobile or prepaid number (no VOIP, one
  account per number, six months before an unlinked number can move), and a
  ticket. ARAM Clash drops the ranked requirement. Riot's FAQ says "There is
  no Honor Level restriction", while its SMS page still says "Your
  highest-ranked account must be Honor level 2". Say both, sourced.
- **[UNIQUE INSIGHT] Tickets per player, and gifts.** Every member submits a
  ticket of their own choosing, and each player's rewards follow their own
  ticket: "A teammate can offer a ticket to anyone else on their team
  roster." Basic tickets cost Blue Essence or RP, Premium more RP, and "every
  two tournaments (once per theme), completing the Clash mission unlocks a
  ticket." The choice is fixed when the full team locks in.
- **[FRESHNESS] The 2026 calendar, alternating maps.** Twelve cups, one a
  month, alternating Summoner's Rift and ARAM, team formation from Monday
  (11:00 local time, by the patch notes) and play on Saturday and Sunday,
  "~4-7 PM Local Time, varies by region". Riot moved January's first cup a
  week, cancelled the 26.2 cup and ran a free makeup in EU, APAC and OC on
  7 February, which is why Riot says "always check in-client".
- **[DATA] TeamTavern's Clash posts.** By tier, and by the roles groups ask
  for against the roles players offer: Top and Jungle are the gap. One HTML
  bar chart (see "Adding TeamTavern's data").

## Content outline

### Introduction

- **Answer first:** Clash is League's monthly weekend tournament for teams of
  five: eight-team brackets at your team's tier, three games a day, on
  Summoner's Rift or ARAM by turns. Every player needs level 30, SMS
  verification and a ticket of their own, and for Summoner's Rift, ranked
  placements this year or last. A team's tier leans toward its best player,
  not the average.
- **Who it's for:** a player with a five, or part of one, who wants to enter
  Clash with players they chose.
- **It covers:** when Clash runs, how a Clash day works, tiers, what each
  player needs, and finding a team. Then the join guide link.
- In short (five or six bullets): the cadence and the next cup; the bracket
  and three games a day; tier and lock-in; the requirements; tickets; finding
  players. Each bullet stands alone, with its year where it's a 2026 fact.

### H2: When Clash runs

- Cover image. Once a month, alternating Summoner's Rift and ARAM, by Riot's
  FAQ. Team formation opens on the Monday of Clash week, at 11:00 local time
  by the patch notes, and the tournament is that Saturday and Sunday.
- **The one table:** the 2026 calendar from Riot's FAQ: Cup | Map |
  Tournament days. Team formation is always the Monday before, so it goes in
  the sentence above the table; a fourth column overflows a 375 px window. Twelve rows, Demacia Cup (Summoner's Rift,
  19 January, 24–25 January) to the Freljord Cup (ARAM, 14 December,
  19–20 December). The Worlds Clash, 16 and 21–22 November, is the next.
  January's cup moved a week and the 26.2 one was cancelled, so the table's
  January row is Riot's moved date.
- Time of day: Riot's patch notes give the tournament days as "~4-7 PM Local
  Time, varies by region". The Clash tab shows the exact time for your server.
  No `<time>` elements: the times are local to each server, not fixed in UTC,
  and Riot publishes no per-server table.
- Dates change: say so with the 2026 examples (moved, cancelled, made up for
  free), and that the client is the place to check.

### H2: How a Clash day works

- Lock-in: two hours, by tier (the four steps above); SEA's one hour. The
  whole team has to lock in; when all five have, the team is in a bracket.
- The bracket: eight teams of one tier, single elimination, three wins take
  the trophy; first-round losers play a consolation bracket. Every team plays
  three games. "Teams of different tiers won't meet during the same weekend".
- Once a day per team, locked in for the whole bracket; Saturday, Sunday or
  both, with the same team or a new one.
- Scouting: opponents see your ranked win rates, KDAs, most-played champions,
  mastery and Clash history. One line: they'll know your champion pools.
- The draft: tournament draft with two ban phases and two pick phases, and
  "the team captain makes all the ban decisions". All champions unlocked. No
  hover locks a random champion. No /pause or /remake. Sudden death from 60
  minutes (40 in ARURF).
- Rewards, briefly: 400 VP a win, a first-win bonus by tier (150, 100, 50,
  none), a trophy for the winners, an orb or capsule by record and ticket.
  Link Riot's FAQ for the tables.

### H2: Clash tiers

- Tier I to IV, I the highest, "bottom-weighted", so most teams are in Tier
  IV. A player's tier comes from their Flex or Solo/Duo rank and their Clash
  history; a team's is a weighted average that "heavily prioritizes the
  highest-skill players". The tier changes as players join or leave the
  roster, and the client warns of a mixed team.
- Placement: from last season's Clash MMR, or ranked MMR for a player new to
  Clash.
- What it means: a Diamond in a Gold five lifts the team toward Diamond's
  teams, and every Gold in it plays against them. Recruit near your rank.
- **The chart:** TeamTavern's Clash posts (see below).

### H2: What each player needs

- Summoner's Rift: level 30; placements in a Summoner's Rift ranked queue
  this year or the year before; SMS verification. ARAM and ARURF: level 30
  and SMS, no ranked.
- SMS: mobile or prepaid numbers only, no VOIP such as Google Voice; one
  account per number; an unlinked number waits six months before another
  account can use it; verify in the Clash tab or Settings. Riot: SMS-link
  only your highest-ranked account.
- Honor: the FAQ and the SMS page disagree; say both, and that the FAQ is
  Riot's main Clash page.
- Tickets: Basic for Blue Essence or RP, Premium for more RP, one from the
  Clash mission every two tournaments, and teammates can give them. Each
  player picks their own, and their rewards follow it. No prices: Riot's
  pages give none (see Not confirmed).
- The captain: creates the team from the Monday, invites and removes
  players, picks name, logo and tag; the ban decisions in the draft are
  theirs too.

### H2: Finding a team for Clash

- Recruit for one cup: its weekend, your tier, the positions you're short
  of, and someone who'll stay for three games.
- **In the client:** since patch 10.18 (2020) a player can set themselves as
  a free agent in the Clash hub, with position preferences, so captains can
  add them. Riot's notes called it a beta then; whether it works the same
  today isn't confirmed, so attribute it to 10.18.
- **Positions:** a five needs Top, Jungle, Mid, ADC and Support. Among
  TeamTavern's Clash posts, Top and Jungle are asked for most and offered
  least. Say what you play.
- **Same server:** a team plays on one server.
- **Keep a good five:** a Clash team that worked is worth keeping for next
  month; How to make an esports team covers keeping a roster together.
- **Post on TeamTavern:** a player post with your rank, "Role", "Clash tier",
  "Clash" ticked under "Looking for", and your Riot ID; a group short of
  players puts up a group post with a "Clash tier" range and the roles it's
  missing. Filter for "Clash" and message the posts at your tier.
- Feed link: `[League of Legends players and groups on TeamTavern](/games/league-of-legends)`.

### FAQ

1. **When is the next Clash in League of Legends?** The Worlds Clash on
   Summoner's Rift, 21–22 November 2026, with team formation from 16
   November; then the Freljord Cup on ARAM, 19–20 December. Riot posts 2027's
   dates in its Clash FAQ.
2. **What time does Clash start?** About 4 to 7 PM local time on the
   tournament days, by Riot's patch notes, varying by server; the Clash tab
   shows yours. Lock-in opens two hours before, and lower tiers can lock in
   earlier.
3. **How does Clash work in League?** Five-player teams, eight-team brackets
   at one tier, three games a day including a consolation bracket.
4. **How are Clash tiers determined?** From each player's Flex or Solo/Duo
   rank and Clash history, weighted toward the team's best player.
5. **What are the requirements for Clash?** Level 30, SMS verification, a
   ticket, and for Summoner's Rift, ranked placements this year or last.
6. **Do you need Honor level 2 for Clash?** Riot's FAQ says no; its SMS page
   says the highest-ranked account must be Honor 2.
7. **Can you play Clash if you're unranked?** ARAM Clash, yes. Summoner's
   Rift Clash needs placements in a Summoner's Rift ranked queue this year or
   last.
8. **What's the difference between a Basic and a Premium Clash ticket?** Only
   the rewards; each player picks their own.
9. **Can you play Clash without a team?** Not alone: a full five locks in.
   Set yourself as a free agent in the Clash tab, or find a team on
   TeamTavern.

### Closing H2: Find your five

Bullets: check the next cup in the Clash tab; get all five SMS-verified and,
for Summoner's Rift, placed; recruit near your rank, since the best player
sets the tier; bring five who'll play three games. Then the call to post and
the feed link. Riot runs no official League Discord that the site has
confirmed; check before writing, and name none if there isn't one.

## Facts, with sources

Checked 2026-11-05.

**Riot's [Clash FAQ](https://support.riotgames.com/en-us/league-of-legends/events/clash-faq), 7 May 2026:**

- "Clash is a monthly tournament. This competitive game mode sees you and
  four fellow players team up to compete against other teams at your skill
  level."
- "You can begin forming your team on Monday of Clash Week, but matches will
  take place on Saturday and/or Sunday. Your team can join an 8-team bracket
  on either day (or on both!) If you win all three games, you'll get a trophy"
- "Your team can only join one Clash bracket each day, and once you've
  joined, that team is locked in for the entire bracket. However, if things
  don't go well for you on Saturday, you can still come back Sunday and try
  again (with the same team, or even a new one!)"
- 2026 schedule (cup, season, team formation, tournament days): Summoner's
  Rift Demacia Cup, S1A1, 19 Jan, 24–25 Jan; ARAM Ionia Cup, S1A1, 16 Feb,
  21–22 Feb; Summoner's Rift Noxus Cup, S1A2, 16 Mar, 21–22 Mar; ARAM Ixtal
  Cup, S1A2, 13 Apr, 18–19 Apr; Summoner's Rift Zaun Cup, S2A1, 4 May,
  9–10 May; ARAM The Void Cup, S2A2, 15 Jun, 20–21 Jun; Summoner's Rift MSI
  Cup, S2A2, 13 Jul, 18–19 Jul; ARAM Bilgewater Cup, S3A1, 17 Aug, 22–23 Aug;
  Summoner's Rift Bandle City Cup, S3A1, 14 Sep, 19–20 Sep; ARAM Piltover
  Cup, S3A2, 12 Oct, 17–18 Oct; Summoner's Rift Worlds Clash, S3A2, 16 Nov,
  21–22 Nov; ARAM Freljord Cup, S3A2, 14 Dec, 19–20 Dec. "Dates can sometimes
  change, so always check in-client for the latest, most accurate
  information." Its line "Season 3 Clash Tournament dates are going to be
  announced at a later date" sits under a table that already has them; it's
  stale, ignore it.
- requirements for Summoner's Rift: level 30; "Completed placements in at
  least one Summoner's Rift ranked queue this year or the year before"; SMS,
  "only mobile/prepaid numbers will be accepted. Internet numbers [VOIP] like
  Google Voice will not work." "ARAM and ARURF Clash still have the same
  account level 30 and SMS verification requirements, but you don't have to
  play any Ranked to participate." "There is no Honor Level restriction for
  Clash."
- captain: "responsible for adding or removing members, and setting up their
  team for Clash. Captains can start inviting their squad from the Monday
  before a Clash tournament", picks name, logo and tag
- tier: "Tier I is the highest, and Tier IV is the lowest. Tiers are based on
  everyone on the team's Rank, but it is weighted more heavily towards the
  best player on the team. Your Tier determines who you'll be matched against
  but also when you can lock-in"; "bottom-weighted, so there'll be more
  players in Tier IV. You'll see your team's tier adjust as you add or remove
  players."; "Individual tiers are determined by your Flex or Solo/Duo
  ranking … you'll also have a rating that is derived from your previous Clash
  history. Team tiers take a weighted average of the roster, but it heavily
  prioritizes the highest-skill players—so it's likely that comps of players
  with vastly different skills may have some tougher matches ahead. You'll see
  a popup if your team has a mixed skillset."; "Teams of different tiers won't
  meet during the same weekend"
- placement: "based on your previous season's Clash MMR. If you've never
  played Clash before or didn't play last season, you'll be placed according
  to your Ranked MMR. Unfortunately, you cannot participate in Clash if you
  were unranked last season." The last sentence contradicts "this year or the
  year before"; use the requirement list.
- tickets: "Every team member must submit at least one ticket"; "Basic or
  Premium Ticket … Each teammate chooses their own Ticket entry and their
  individual rewards scale off that. Once your Basic or Premium Ticket is set
  (when your full team locks in before a match), your entry can't be
  changed."; "You can get tickets from missions, a teammate, or the store.
  Every two tournaments (once per theme), completing the Clash mission
  unlocks a ticket. A teammate can offer a ticket to anyone else on their
  team roster. In the store, you can purchase Basic Tickets with Blue Essence
  or RP, and Premium Tickets at a higher RP cost."; "The only difference
  between Basic and Premium Tickets is the prize pool"
- SMS and smurfing: one number, six-month wait after unlinking; "If you play
  on multiple accounts, be sure to only SMS link your highest-ranked account."
- bracket: "single-elimination 8-team bracket against teams at the same skill
  level"; "a consolation (lower) bracket held for teams who lost the first
  match of each day"
- lock-in: "Tier IV can lock-in at any time of the two-hour lock-in window,
  Tier III can lock-in a half hour later, Tier II can lock in after another
  half hour, and Tier I can only lock-in for the last half hour."; SEA: "all
  tiers can lock in 1 hour before Clash begins."
- scouting: "your opponents' win rates and KDAs with their most-played
  champions in ranked, their best champs in terms of mastery, and their
  previous Clash data"
- draft: "two ban phases, and two pick phases … The team captain makes all the
  ban decisions for the team."; "All champions are unlocked in Clash"; "not
  declaring or hovering a champion will lock in a random champion for you"
- sudden death: "After 60 minutes (40 minutes for ARURF), towers will become
  more fragile"; "/pause and /remake are disabled in Clash"
- rewards: 400 VP a win; first-win bonus 150 / 100 / 50 / none for Tiers I to
  IV, once per Clash day; orb and capsule tables by 0–3, 1–2, 2–1, 3–0; a
  trophy for winning the bracket, shown at the nexus for two weeks

**Riot's [SMS Verification page](https://support.riotgames.com/en-us/league-of-legends/events/clash-sms-verification),
22 May 2026:** verify in the Clash tab ("Team", then "Confirm SMS"); "can
only attach one account to your phone number"; six-month wait for another
account after unlinking, while "you can relink it to the same account at any
time"; "Your highest-ranked account must be Honor level 2 in order for you to
participate in Clash." Its date doesn't say which line changed; the Honor
line contradicts the FAQ.

**Riot's patch notes, 2026** (`patches.txt`): registration "@ 11:00 AM (Local
Time)" and tournament days "(~4-7 PM Local Time, varies by region)" in every
Clash section (26.1, 26.2, 26.4, 26.6, 26.8, 26.14). [26.1](https://www.leagueoflegends.com/en-us/news/game-updates/patch-26-1-notes/):
the first cup moved back a week, to 24–25 January.
[26.3](https://www.leagueoflegends.com/en-us/news/game-updates/patch-26-3-notes/):
"For players in EU, APAC, and OC we're going to be running a makeup
Summoner's Rift Clash since we unfortunately had to cancel the 26.2 patch
Clash", on 7 February, "free to enter, with a premium ticket option".
[26.9](https://www.leagueoflegends.com/en-us/news/game-updates/league-of-legends-patch-26-9-notes/):
WASD enabled for Ranked, "including Clash" (leave out). No notes after 26.14
(the MSI Cup, July) mention Clash; the August to October cups appear in the
FAQ's table and in wards.lol's history from the Riot API, but no Riot page
read confirms that they were played.

**[Patch 10.18](https://www.leagueoflegends.com/en-us/news/game-updates/patch-10-18-notes/), 2020:**
"BETA: Rather than creating your own team, visiting the Clash hub and setting
yourself as a Free Agent allows other teams that are looking for players to
add you to their rosters. Set your position preferences and get in on the
action." Also "new tooltips in the Clash hub that explains how players can
form or find a team".

**The community wiki** (marked Outdated; don't cite for current facts): a
Basic Ticket "can be bought with 975 BE or 195 RP", a Premium for 975 RP,
sourced to the 2019 European beta; a substitute can stand in "for a single
day, but only after the lock-in phase begins", in the same tier; the team
must lock in "within 30 minutes of match start" or its tickets are
returned. None of it is in Riot's 2026 pages.

**What ranks** (web search results, 2026-11-05): lolnow.gg's schedule,
checked 8 September, gives the Bandle City Cup as next; 1v9.gg and
turbosmurfs show only early-2026 dates; bo3.gg and older explainers say a
team's bracket comes from "the average of all team members" and that Honor
level 2 is required; one says "Riot reduced the 2025 schedule from 12 to six".
wards.lol's schedule gives lock-in windows "derived from Riot lock-in rules"
that don't fit its own start times; don't use.

## Not confirmed

- **Ticket prices.** Riot's pages say Blue Essence or RP, not how much. The
  wiki's 975 BE / 195 RP and 975 RP come from 2019; LoL Theory (August 2026)
  gives 975 BE. Leave numbers out.
- **Honor.** The FAQ says no restriction; the SMS page says Honor 2 for the
  highest-ranked account. Say both.
- **Exact times per server.** Riot's notes give "~4-7 PM Local Time, varies
  by region"; whether that's lock-in, the first game or the whole evening
  isn't said. Quote it and send the reader to the Clash tab.
- **Free agents today.** Added as a beta in 10.18 (2020); no current Riot
  page mentions it. Attribute it to 10.18. Autocomplete's "clash find a team
  greyed out" suggests a Find Team control in the client; no source read
  describes it.
- **Substitutes.** Riot's 2026 pages say a team is "locked in for the entire
  bracket"; the wiki's day-long substitute rule is from the beta. Leave subs
  out beyond that.
- **How ARAM Clash sets tiers** without ranked. Not said; leave out.
- **Whether the August to October 2026 cups ran.** The FAQ lists them and the
  Riot API had them (wards.lol), but no patch note mentions them. The table
  gives Riot's schedule as Riot's schedule.
- **An official League of Legends Discord.** Not checked.

## Adding TeamTavern's data

**Counted while writing,** from `team_tavern` (newest post 26 September 2026),
`%LOCALAPPDATA%/Temp/clash/posts.sql`:

- "Clash" under "Looking for": 1,004 of 3,639 player posts (28%), 141 of 344
  group posts (41%), 31 communities, January 2021 to August 2026.
- "Clash tier" on 571 of those player posts: Tier IV 203, Tier III 100, Tier
  II 97, Tier I 171. Self-reported, and more Tier I than Riot's
  "bottom-weighted" tiers would give; say "players say they're in", not
  "players are in".
- 86 Clash group posts give a tier range; 16 span Tier IV to Tier I, 43 stay
  within one tier.
- Roles, among Clash posts that name one: groups ask for Top 62%, Jungle 61%,
  Mid 53%, ADC 50%, Support 52%; players offer Top 36%, Jungle 40%, Mid 43%,
  ADC 45%, Support 50%.

**The chart:** the roles, groups asking against players offering, in League's
lane order, as the join guide's role chart is drawn, captioned "TeamTavern's
Clash posts, 2021–2026, counted <date>". Top and Jungle are the gap. The tier
counts go in a sentence in the tiers H2. Recount from a fresh backup if the
user provides one.

## Internal links

- **From this guide:** `/games/league-of-legends` twice (the recruiting H2 and
  the close); the join guide from the introduction
  (`/guides/join-an-esports-team#your-games-team-format`); the ranked guide
  from the tiers H2 or the requirements H2, by its H1 ("What ranks can duo in
  League of Legends, and playing ranked with friends"), for placements and
  for a five that also plays Flex or Ranked 5s; the make guide from the
  recruiting H2.
- **To this guide:** the join guide's "League of Legends: Clash" H3 and the
  ranked guide's "From ranked to a team" bullet, which now points at the
  join guide's Clash section.

## Adding to the join and make guides

- **Join guide, table row:** keep "**League of Legends: Clash**, 5 players |
  Level 30, ranked placements this year or last, SMS on a mobile number, and
  a ticket each, for Blue Essence or RP or from the Clash mission | 12
  weekends in 2026"; add "(ARAM Clash needs no ranked)" if it fits.
- **Join guide, the League H3:** keep the cover, the "doesn't lead to pro
  play" paragraph and the feed link; add that a team's tier leans toward its
  best player and every team plays three games a day, and link this guide by
  its H1.
- **Make guide, table row:** keep it; it already matches the FAQ. Its Clash
  paragraph under "Set up the team" stays.
- **Ranked guide:** point "From ranked to a team" at this guide by its H1,
  and move its `updated`.
- Move the join and make guides' `updated` only where a sentence changes.

## Slug, title, description

- **Slug:** `league-of-legends-clash`
- **H1:** How League of Legends Clash works, and how to find a team for it
- **Title:** LoL Clash: schedule, tiers, tickets and how to find a team
- **Description:** A monthly weekend bracket for teams of five, three games a
  day at a tier set mostly by your best player, with a ticket and SMS
  verification for every player: how League of Legends Clash works, and how
  to find a five for it.

## Keeping it current

- **After 22 November:** the FAQ's first answer turns to the Freljord Cup.
- **When Riot posts 2027's schedule** in the Clash FAQ (when it went up for
  2026 isn't known; check from early January): the table, the FAQ answer and
  the In short bullet. The slug outlives the
  year.
- **When the FAQ's date moves:** compare the requirements, tier, lock-in and
  ticket paragraphs.
- **Each patch with a Clash section:** check its times and anything new.
- Change `updated` only when a fact changes.
