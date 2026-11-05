# Content Brief: How the Dota 2 Battle Cup works, and how to find a team for it

The fourteenth guide, and the first for Dota 2. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the Rocket League tournaments brief, which is its nearest model: a weekly in-game bracket with a schedule. It covers what a player needs to know to enter the Battle Cup with a five they chose:

- when each division's cup starts, in the reader's own time
- how the bracket works, and what happens when a team misses a deadline or no opponent turns up
- tiers: whose tier a team plays at, and that it can go up but not down
- what each of the five needs: Ranked calibration, and Dota Plus or a ticket of their own
- how to find four players who fit, in the client and on TeamTavern

Dota 2 is already on the site, so the guide goes out as soon as it's written, and the join and make guides then link here.

Prepared 2026-11-05. Evidence comes from:

- Google autocomplete for 46 Battle Cup and Dota team seeds (`%LOCALAPPDATA%/Temp/gsc/suggest-battle-cup.mjs`, output `suggest-battle-cup.txt`)
- the Google results and AI Overview for "dota 2 battle cup time", loaded on 2026-11-05 with `hl=en&gl=us` in a profile placed in Croatia
- **the client's own text**: `dota_english.txt` from SteamDB's [GameTracking-Dota2](https://github.com/SteamDatabase/GameTracking-Dota2), as of the build tracked on 2026-11-03 (copy in `%LOCALAPPDATA%/Temp/dota-bc/`)
- Valve's [Dota Plus page](https://www.dota2.com/plus), undated, read 2026-11-05
- Valve's Battle Cup FAQ, `dota2.com/battlecup`, which now redirects to the home page; its last Wayback copy is [3 March 2018](https://web.archive.org/web/20180303014220/http://www.dota2.com:80/battlecup)
- [Liquipedia's Weekend Battle Cup page](https://liquipedia.net/dota2/Weekend_Battle_Cup), last edited 7 August 2026, and [esports.ru's Battle Cup page](https://esports.ru/dota-2/wiki/battle-cup-dota-2-vsya-informaciya-o-ezhenedelnom-sobytii/), updated 3 October 2025
- the 82 Battle Cup reports in Valve's [Dota2-Gameplay issue tracker](https://github.com/ValveSoftware/Dota2-Gameplay/issues), 2022 to 26 October 2026 (`issues.tsv`), and the 46 client screenshots attached to the 2025–2026 ones (`%LOCALAPPDATA%/Temp/dota-bc-shots/`)
- [OpenDota](https://www.opendota.com)'s match records: the reported Battle Cup matches by ID, and every Battle Cup match (lobby type 9) recorded on Saturday 24 October 2026, by UTC hour and server cluster (`sweep.mjs`, output `sweep-1024.txt`)
- r/DotA2 threads titled "Battle Cup", April to November 2026, read in the browser, never linked
- the local cluster's production copy, `team_tavern`, whose newest post is from 26 September 2026

Valve's 300 Steam announcements for Dota 2 since June 2020 never mention the Battle Cup, and no Valve page gives its schedule. The client is the primary source, so the guide links Valve's Dota Plus page and Liquipedia, and states the client's rules as what the client says.

## Why this topic

The user chose it on 2026-11-05 from the next-guide check (GUIDES.md, "Topics not written yet"):

- **A rules-shaped question with a schedule.** Autocomplete is full for times ("dota 2 battle cup time" with EST, PST, Europe and SEA forms, "registration time", "start time", "when is battle cup", "what time is battle cup"), tiers ("tiers", "tier mmr", "team tier", tier 4, 6, 7 and 8 each), "rewards", "prize", "ticket price", "dota plus", "how does battle cup work" and "how to join battle cup".
- **It's what Dota posts on TeamTavern are for.** 38% of Dota 2 player posts and 53% of group posts tick "Battle Cup" under "Looking for", and the seed has a "Battle Cup tier" field.
- **What ranks is wrong.** The AI Overview says a team needs "a Dota Plus subscription (or 5 Battle Cup tickets)", as Liquipedia does. The client says Dota Plus covers only the member who has it: every other player needs a ticket.
- **It's running.** Valve's tracker has Battle Cup reports from every few weeks through 2026, nine of them from Saturday 24 October.

Dota's party MMR gap stays out: Valve publishes none.

## Template

**Recommended**: `how-to-guide`, shaped like `rocket-league-tournaments.md`. The reader wants to know when the cup is, whether their stack can enter it and at which tier, and how to fill the slots they're short of.

## Target keywords

- **Primary:** dota 2 battle cup, and how does battle cup work
- **Secondary:**
  - battle cup time, schedule, start time and registration time, with "europe", "sea", "est" and "pst"
  - battle cup tiers, tier mmr, team tier
  - battle cup ticket, ticket price, dota plus
  - how to join battle cup
  - battle cup rewards, prize, trophy
- **Leave to the feed:** "dota 2 battle cup team finder", "dota 2 team finder", "lfg", "party finder"
- **Leave out:** drafting and gameplay; Battle Pass-era cups, the Champions Cup and its Major qualifier slots (2016–2018); Cup Score values (the client's tooltip, 10/20/40/70, and the wikis, 10/20/30/90, disagree); party MMR in Ranked.
- **Questions:** see the FAQ below.

## Search intent

Informational. The reader has a stack, or half of one, and wants to know when to be online, whether everyone can enter, and what tier they'll land in.

What ranks:

- **Liquipedia and the Fandom wiki** hold the schedule table everyone else copies: Saturday, Southeast Asia 11:00, China 12:00, Europe 18:00, Americas 01:00 UTC on Sunday. It misses South America's cup at 23:00, and the entry fee they give, a Dota Plus subscription or five tickets per team, is wrong.
- **Boosting and smurf sites** (teamsmurf, immortalboost, epiccarry, buyboosting) rank for times and tiers. teamsmurf's "Battle Cup October 2026 guide" puts the cup on "the final Saturday and Sunday of October" and its autumn calendar lists four weekends, both wrong.
- **A Steam discussion from 2017** answers "registration time" with "15 min to set the team, got 30 min to search a game".
- **The AI Overview** takes Fandom's times and the five-ticket rule.

None says that each player without Dota Plus needs a ticket of their own, that one Plus member covers only themselves, that the roster can't change once entered, or how the client's party builder and division chat find teammates.

## Information gain

- **[UNIQUE INSIGHT] Who pays.** By the client's entry tooltip: "The Battle Cup is free for Dota Plus members. For each player without a membership, one ticket must be purchased. Each player may contribute any number of tickets, as long as the team total of Dota Plus members + tickets is 5." So a stack with one Plus member buys four tickets, and the captain can buy them all.
- **[UNIQUE INSIGHT] The highest tier decides, and only up.** "You must select a tier at least as high as the maximum tier among all team members." A Tier 3 player in an Immortal's stack plays Tier 8. Recruit inside one tier, or accept the top player's.
- **[UNIQUE INSIGHT] Five, fixed, once a weekend.** The roster is set once the team enters (2018 FAQ), only the party leader can forfeit for the team, nobody can abandon a cup match ("your team can surrender by chatting 'GG'"), and a player who has "already played in the Battle Cup this weekend" can't play again. So a stack needs five who'll stay for three matches, and a stand-in found that evening can't have played elsewhere.
- **[UNIQUE INSIGHT] The deadlines.** Miss a queue deadline and, if no opponent is found, the team forfeits its entry fee. Meet it and no opponent is found, and the team gets a bye, the title or a refund. Players report waits of 20–25 minutes before a refund in 2026.
- **[FRESHNESS] The schedule, from the matches themselves.** Every source that ranks gives four divisions with one "Americas" cup at 01:00 UTC. OpenDota's records of 24 October 2026 show five: Southeast Asia 11:00, China 12:00, Europe 18:00, South America 23:00 and North America 01:00 UTC, each starting on the hour. Europe started at 18:00 UTC in December 2024 too, so the times are fixed in UTC and move an hour on local clocks where they change.
- **[DATA] TeamTavern's tiers.** Player posts that give a Battle Cup tier: Tier 3 187, Tier 4 138, Tier 5 126, Tier 6 118, Tier 7 155, Tier 8 212 (936 posts, 2019 to 26 September 2026). Tier 8 is the commonest and Tier 3 next. One HTML bar chart.

## Content outline

### Introduction

- **Answer first:** The Battle Cup runs every Saturday, one eight-team bracket per tier in each division, and three wins take it. A team plays at its best player's tier, and Dota Plus pays only for the member who has it: each of the other four needs a ticket.
- **Who it's for:** a player with a stack, or part of one, who wants to enter the Battle Cup with four players they chose, not a party builder's pick.
- **It covers:** when it runs, how the bracket works, tiers, what each player needs, and how to find the rest of the five. Then the join guide link.
- In short (five bullets): the schedule; the bracket; the tier rule; the entry fee per player; the locked roster and once-a-weekend rule.

### H2: When the Battle Cup runs

- Cover image. Every Saturday, by division. The client's Battle Cup tab shows each division's next start in local time.
- **The one table:** Division | Starts | Servers. Southeast Asia, Saturday <time>11:00</time>, Singapore and Australia; China, Saturday <time>12:00</time>; Europe, Saturday <time>18:00</time>, Western and Eastern Europe and Stockholm; South America, Saturday <time>23:00</time>, Brazil, Chile, Peru and Argentina; North America, Sunday <time>01:00</time>, US East and US West, which is Saturday evening there. Source: OpenDota's records of 24 October 2026. A `<time>` that crosses midnight in the reader's zone shows the wrong day beside it, so the day column is in UTC and the note says so.
- The `guide-note` under it, as in the RL guide.
- The times are fixed in UTC: Europe started at 18:00 UTC in December 2024 as in October 2026. So where clocks change, the cup moves an hour on the local clock (Europe: 20:00 in Central European summer, 19:00 in winter).
- Any division can be chosen, each with its own time and servers, picked by ping; the client says "smaller server regions are not used for Battle Cup games". A player can play once a weekend, so a team picks one division.
- Teams register before the start, and the first matches begin on the hour. The window's length isn't confirmed, so the guide says to be partied up and registered before the hour.

### H2: How the Battle Cup works

- Eight teams, single elimination: quarterfinal, semifinal, grand final. Three wins take it. Every match is Captain's Mode (game mode 2 in every Battle Cup match on OpenDota, as Liquipedia says), so the team needs someone to draft.
- How long: on 24 October, Europe's quarterfinals started from 18:00 UTC, semifinals from 19:00, finals from 20:00, and the last finals by about 21:20. A team that goes all the way is busy for about three hours.
- Opponents come by tier and division, not MMR or language (2018 FAQ; the client's tier tooltip: "Battle Cup matchmaking does not use MMR").
- Between rounds the team queues again before a deadline. The deadline rules, from the client and the 2018 FAQ.
- No abandoning a cup match: surrender with "gg". Only the party leader can forfeit the cup, and that loses the entry fee.
- Disconnects: the game starts without a player who can't connect, and there's no AFK detection (2018 FAQ; say so).
- Rewards for the winners: 20,000 Shards each for Dota Plus members (Valve's Plus page; the client says "per player"), and by Liquipedia a trophy, chat badge and emoticons for a week and a Battle Cup effigy.
- Opponents in later rounds can see a team's earlier games in the bracket (players report). One line, since it's why a team shouldn't lean on one hero pool; no more, as that's gameplay.

### H2: Battle Cup tiers

- Each player has a tier, 3 to 8, from their rank. Liquipedia and esports.ru agree: Tier 3 Crusader and below, 4 Archon, 5 Legend, 6 Ancient, 7 Divine, 8 Immortal. The client's hub shows your Current Tier.
- The team's tier is at least its highest player's; the party leader can enter higher, never lower. The client's party builder won't run for a team that raised its tier.
- What it means: recruit within your tier, or a Tier 8 friend takes a Tier 4 stack into Tier 8.
- **The chart:** TeamTavern player posts by Battle Cup tier.

### H2: What each player needs

- Every one of the five must have calibrated for Ranked (client), which needs hours of unranked play first: the same check, "at least {d:required_play_time} hours of unranked", which Liquipedia puts at 100.
- Entry: Dota Plus, or a ticket per player without it. $0.99 a ticket on Valve's Plus page. Anyone can buy tickets for the others, and Plus can be gifted (client).
- The party leader registers, names the team and picks its logo, as a pick-up team or an official team, which shows on players' profiles.
- One cup per player per weekend. The roster is set once the team enters: a player who leaves isn't replaced, and players report teams playing a whole cup four against five after a stranger filled in and left before the first game.

### H2: Finding a team for the Battle Cup

- Recruit for one slot: your division's Saturday, your tier, the positions you're missing, and someone who'll stay for three matches.
- **In the client:** the party builder ("Find party" for a solo player, "Find player" for a party), which matches by language, tier and ping and only works for players who own a ticket and teams that haven't raised their tier; and the chat channel for each division and tier, open to Dota Plus members and ticket holders.
- **Positions:** a stack needs 1 to 5 covered. The join guide's chart has mid as the scarcest. Say what you play.
- **Keep the good ones:** a stand-in who played well is a friend request away; fixed rosters make a regular five worth more than a fill.
- **Post on TeamTavern:** a player post with your rank, "Battle Cup tier", "Position" and "Server", "Battle Cup" ticked under "Looking for", and your division and Saturday in the description. Add your Steam or Discord. A stack short of players puts up a group post the same way. Filter for "Battle Cup" and message the posts at your tier. 38% of player posts and 53% of group posts in Dota 2 tick it.
- Feed link: `[Dota 2 players and groups on TeamTavern](/games/dota-2)`.

### FAQ

1. **What time is Battle Cup in Dota 2?** Saturdays, per division: Southeast Asia 11:00, China 12:00, Europe 18:00 and South America 23:00 UTC, and North America at 01:00 UTC on Sunday, Saturday evening there.
2. **How does Battle Cup work?** An eight-team single-elimination bracket of teams at the same tier in one division; three wins take it.
3. **What tier am I in Battle Cup?** By rank: Tier 3 for Crusader and below up to Tier 8 for Immortal. A team plays at its highest player's tier or above.
4. **Do all five players need Dota Plus for Battle Cup?** No. Each player has Dota Plus or a ticket, and anyone on the team can buy the tickets.
5. **Can you play Battle Cup solo?** Not alone, but the client's party builder puts a solo player with a ticket into a party at their tier.
6. **Why can't I join Battle Cup?** Not calibrated for Ranked, not enough unranked hours, already played this weekend, or registration closed.
7. **Can you change players during a Battle Cup?** No. The roster is fixed once the team enters.
8. **What do you get for winning Battle Cup?** Shards for Dota Plus members, a trophy and a badge for a week.

### Closing H2: Find your five

Bullets: pick one division's Saturday; recruit inside your tier; check every player is calibrated and has Plus or a ticket; bring five who'll play all three matches. Then the call to post and the feed link. No official Dota 2 Discord turned up; check once more before writing, and name none if there isn't one.

## Facts, with sources

Checked 2026-11-05.

**The client (`dota_english.txt`, 2026-11-03). State as what the client says; link [Valve's Dota Plus page](https://www.dota2.com/plus) for the price and the Plus perk:**

- "8-team single-elimination tournament"; rounds "Quarter Finals", "Semi Finals", "Grand Final"
- tier: "Select a skill level. Battle Cup matchmaking does not use MMR. You must select a tier at least as high as the maximum tier among all team members."
- entry: "The Battle Cup is free for Dota Plus members. For each player without a membership, one ticket must be purchased. Each player may contribute any number of tickets, as long as the team total of Dota Plus members + tickets is 5." Buying for others: "You can buy tickets on behalf of party members who are not Plus members. You can also give them a Plus membership as a gift."
- eligibility: "Each team member must have calibrated for Ranked."; "all team members must be have played at least {d:required_play_time} hours of unranked matches" (Ranked's check uses the same variable)
- once a weekend: "Team member has already played in the Battle Cup this weekend."
- leader: "Only the party leader can initiate Battle Cup registration."; "Party Leader picks team name and logo"; "Only the party leader may act on behalf of the team to forfeit the Battle Cup"
- no abandoning: "You cannot abandon the current Battle Cup match, but your team can surrender by chatting 'GG'"
- teams: "Pick-up teams don't appear up on your profile"; official teams "appear up on your profile"
- deadlines: "If you miss this deadline and cannot find an opponent, your team will forfeit its entry fee."; outcomes "Entry fees were refunded", "Your team was declared the tournament champion", "Your team received a bye in the Semi Finals"
- party builder: "Find party" ("Find a party near you at your tier and join it"), "Find player" ("Find a player near you at your team tier and add them to the party"); not available "because you've chosen to increase the tournament tier"; "only available for players who already own a ticket"
- chat: "Join {division} Tier {n} Chat"; "only players who currently own a Dota Plus subscription or ticket may join"
- servers: "Server regions are automatically selected based on ping time… smaller server regions are not used for Battle Cup games."
- divisions named in the client: Americas, Europe, Southeast Asia, China, North America, South America (some may be unused)
- prize: "Championship prize: {n} shards per player"
- an "offseason" state exists ("The Battle Cup is in offseason."), as do free weekends

**Valve's [Dota Plus page](https://www.dota2.com/plus), undated:** "The weekly Battle Cup is back. And it's free for all Dota Plus members. Non-members can still buy Battle Cup tickets for $0.99 in order to participate. The prize for winning, in addition to the special emoticons and profile accolades, will be 20,000 Shards." It shows the membership in local currency (€3.50 a month from Croatia), so the guide gives no membership price.

**Valve's Battle Cup FAQ, [Wayback, 3 March 2018](https://web.archive.org/web/20180303014220/http://www.dota2.com:80/battlecup).** Cite with its year, for what the client confirms or doesn't contradict:

- "Each player has an individual tournament tier, which is set at the start of each season from your MMR… If team members have different tiers, the highest tier is used."
- "Tournament matchmaking uses automatic region selection, and MMR and language preferences are not factored in."
- party builder "matches based on language, tier, and server ping"; the chat channel for "the same tier and geographic division"
- "A team's roster is set once they have entered the tournament, and individual players are not able to leave the team for a replacement."
- "You can select to participate in any geographic division you wish… each division has a designated time slot and assigned game server regions."
- the deadline, bye and refund rules, as the client has them
- disconnects: "the game will start without them… disconnected players are not timed out… there is no AFK detection"; "In the event of a server crash or poor network conditions, all tickets are refunded"
- superseded, don't use: eligibility by "profile badge over level 25" and "no calibrated MMR" (the client now requires Ranked calibration); "each team requires 5 tickets" (now Plus or a ticket per player); "Teams have 30 minutes to enter the queue"; the Champions Cup

**[Liquipedia](https://liquipedia.net/dota2/Weekend_Battle_Cup), last edited 7 August 2026:** Saturdays; Southeast Asia 11:00, China 12:00, Europe 18:00, Americas 01:00 UTC (+1 day); tiers 3 to 8, "Tier 3 Crusaders or below" up to "Tier 8 Immortals"; Captain's Mode; "at least 100 hours in unranked matchmaking"; "Teams must queue for matches within the fifteen minute window after registration"; rewards (trophy and chat badge for a week, prestige profile for a week, seven emoticons for a week, an effigy, 20,000 Shards for Plus members). Wrong: "Each team requires a Dota Plus subscription, or five Weekend Battle Cup Tickets", and one "Americas" cup where there are North and South American ones.

**[esports.ru](https://esports.ru/dota-2/wiki/battle-cup-dota-2-vsya-informaciya-o-ezhenedelnom-sobytii/), 3 October 2025:** every Saturday at 21:00 MSK in Europe (18:00 UTC); the same tiers by medal; the captain can raise the tier but not lower it; the team's tier is its highest player's. Its own list of division times disagrees with its opening (Europe 19:00 MSK), so don't use it for times.

**[OpenDota](https://www.opendota.com)'s match records.** A third party, but the primary source for its own data; cite it for the schedule:

- Saturday 24 October 2026, every Battle Cup match (lobby type 9) by UTC hour and server cluster, through OpenDota's explorer: starts at 11:00 on Singapore and Australia clusters; 12:00 on clusters 413–417 (no region in OpenDota's constants); 18:00 on Stockholm, Austria and Europe clusters; 23:00 on Brazil, Chile, Peru and Argentina; 01:00 on Sunday on US West and US East. Every window's first matches start on the hour. Europe: semifinals from 19:00, finals from 20:00, the last starting 21:20.
- Reported matches by ID: Europe quarterfinals at 18:01 (28 December 2024), 18:01 (16 May 2026, semifinal 18:54), 18:00 (18 July 2026), 18:02 (19 September 2026), 18:09 (24 October 2026); all lobby type 9, game mode 2 (Captain's Mode).
- A North America report of 28 December 2025 gives "8:03 PM EST", 01:03 UTC: the 01:00 UTC start in winter.

**Client screenshots in Valve's tracker** (2025–2026): the cup is titled "Dota Plus Battle Cup", with "Europe Division – Tier 3" to "Tier 8", "Southeast Asia Division – Tier 7" (May 2025) and "North America Division – Tier 6" (December 2025); a bracket of eight with Quarter Finals, Semi Finals, Grand Final and Champion; "Leave Battle Cup" and "Unable to leave the Battle Cup at this time" while a match is on; a division chat channel, "Боевой кубок (Европа …)", with 174 members during a cup.

**Valve's tracker (player reports; state as what players report, don't link):**

- Europe's quarterfinal created "~18:06" UTC on Saturday 24 October 2026 (#36490); a match at "21.12 (UTC+03:00)" the same evening (#36494); EU reports at 18:05 UTC (1 Aug 2026) and 18:31 UTC (4 Apr 2026); a winter one at 18:23 UTC (13 Dec 2025)
- reports on Sundays between 01:00 and 05:00 UTC, which fit the Americas cup
- no semifinal opponent after "20 minutes and 30 seconds" and a refund (Europe, Tier 5, 18 July 2026); about 25 minutes and a refund (Europe, 3 October 2026). A Dota Plus member's "refund" is worth nothing.
- a North America cup in January 2026 (#30450)

**r/DotA2, April to November 2026 (never linked or named):** players look for Battle Cup teammates in the client's chat and ask where else to find them; a team whose stranger left before the first game played the cup four against five (31 October 2026); later opponents ban the heroes a team played earlier, since the bracket shows its games; complaints of smurfs in Tier 3.

**TeamTavern's posts (`team_tavern`, to 26 September 2026):** Dota 2 has 3,204 player, 262 group and 28 community posts. "Battle Cup" under "Looking for": 1,203 player posts (38%), 138 group posts (53%), 17 communities. "Battle Cup tier" on 936 player posts: Tier 3 187, 4 138, 5 126, 6 118, 7 155, 8 212. No group post gives a tier.

## Not confirmed

Nobody on the project has the client, and no recent screenshot or video shows the hub's schedule panel. The match data answers what that panel would have (divisions, times, whether they move with the clocks). Still open:

- **The registration window's length** (15 minutes by Liquipedia, 30 in 2018). Leave it out; say to register before the hour.
- **Tier thresholds by medal** come from Liquipedia and esports.ru, not Valve.
- **The unranked hours.** The client has a variable; Liquipedia gives 100. Say "by Liquipedia's count".
- **Whether Dota Plus counts as "a ticket" for the party builder.** The client says ticket. Say what the client says.
- **How far a team can raise its tier.** The client says "at least as high"; the 2018 FAQ said "the next higher tier". Say it can enter higher, not how much.
- **The 12:00 cup's servers.** OpenDota names no region for clusters 413–417; that they are China's rests on the time matching Liquipedia's China row and the client naming a China division.

## Adding to the join and make guides

- **Join guide, table row:** **Dota 2: Battle Cup**, 5 players | Calibrated for Ranked, and Dota Plus or a $0.99 ticket each | Every Saturday, in each division.
- **Join guide, the Dota 2 H3:** keep the cover and the feed link; say a team plays at its highest player's tier and every player needs Plus or a ticket, and link the guide by its H1.
- **Make guide, table row** under "Set up the team in your game's format": **Dota 2: Battle Cup** | The party leader registers the five and names the team; each player has Dota Plus or a ticket | The team enters at its highest player's tier or above; the roster is set once it enters.
- The make guide's FAQ already gives five for most games. Move both guides' `updated`.

## Slug, title, description

- **Slug:** `dota-2-battle-cup`
- **H1:** How the Dota 2 Battle Cup works, and how to find a team for it
- **Title:** Dota 2 Battle Cup: times, tiers and how to find a team
- **Description:** Every Saturday, an eight-team bracket in each division at your team's highest tier, and a ticket for every player without Dota Plus: how the Dota 2 Battle Cup works, and how to find a five for it.

## Keeping it current

- The schedule: rerun `sweep.mjs` over a recent Saturday's match IDs every few months, and whenever players report a move.
- The rules: the client's strings in GameTracking-Dota2 (`grep -i weekendtourney`), and the Dota Plus page for the ticket price.
- Change `updated` only when a fact changes.
