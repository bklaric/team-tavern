# Content Brief: Marvel Rivals team size, party size and playing ranked with friends

The sixth game guide, and the second for Marvel Rivals. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the Championship, Rocket League, The Finals, Deadlock and Rainbow Six Siege briefs. It takes Competitive's party rules and covers what a player needs to know to play ranked with friends: how big a party can be at each rank, which ranks can queue together and how the game counts the gap, why friends who could queue last season can't during placements, what PC and console do to a party, and how to find a stack inside the gap. Marvel Rivals is already on the site, so the guide goes out as soon as it's written. The join guide, the make guide and the Championship guide then link here.

Prepared 2026-10-21. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, 25 Sep to 19 Oct 2026 (`now-pq.tsv`)
- Google autocomplete for 31 Marvel Rivals ranked seeds (`%LOCALAPPDATA%/Temp/gsc/suggest-rivals.mjs`), and the Google results for "marvel rivals party size", "marvel rivals what ranks can play together", "marvel rivals ranked with friends", "can you play ranked with pc and console on marvel rivals" and "marvel rivals placement matches with friends", loaded on 2026-10-21 with their AI Overviews
- every NetEase patch note from 3 December 2025 to 15 October 2026, 45 of them, searched for party, rank, placement and crossplay changes (`grep-notes.mjs`, output in `rivals-notes.txt`), and the Season 2, 3.5 and 5 notes read in full
- the in-game rank restrictions panel (a screenshot of 2026-01-11) and r/marvelrivals threads from January to October 2026, read only

## Why this topic

Chosen on 2026-10-20 from the next-guide check (see the blog-plans memory):

- **The pattern that works.** Guides that answer how a game's mode treats friends rank at 4–8 within days. Deadlock's leads the site with 12.1k impressions, The Finals' has 3.1k. The lesson from the Deadlock refresh: its biggest queries were "team size" and "how many players per team", which it answered only in passing.
- **Autocomplete is fuller for Marvel Rivals than for any other game.** Ten suggestions each for "party size", "party restrictions", "team size", "ranked with friends", "rank gap", "rank restrictions", "5 stack", "6 stack", "placement matches", "crossplay ranked", "ranked requirements", "pc and console ranked", "how many people can play marvel rivals" and "can you play ranked with" (… 4, 5 and 6 people, pc and console). "What ranks can play together" has eight.
- **The pair questions are the ones a table answers.** "can gold queue with bronze", "can plat queue with silver", "can gold queue with diamond", "can silver queue with gold", "duo rank difference", "rank gap to play with friends".
- **The Marvel Rivals feed has almost no search presence**: 1 impression in the period. The Championship guide has 1.8k, nearly all "how to make a faction". It is already catching the size questions it answers in passing: "how many players on a team in marvel rivals" (position 9), "how many people on a marvel rivals team" (4), "marvel rivals 6 stack rules" (11), "how many people can do placement matches in marvel rivals" (24). This guide gives those a page of their own.

## Overlap with the Championship guide

`marvel-rivals-championship-team.md` has "## Climbing to Platinum 3: who you can queue with" (lines 48–62) and the FAQs "Can PC and console players play Competitive together?" and "Why can't I queue Competitive as a party of five?". Two pages answering the same question split the queries between them.

**Recommended:** this guide owns the party, rank and platform rules. The Championship guide's H2 shrinks to two or three sentences on what they mean for a Faction (members within three divisions climb together, as a full six below Celestial; from Celestial up a Faction can't queue together and practises in custom games), and links here with the anchor "who can queue Competitive together". Its two FAQs go, or become one line each that links here. That's the join guide's pattern: it owns the format facts, and the make guide links back. It changes the Championship guide's `updated`.

The alternative is to leave the Championship guide as it is and accept the overlap. **The user decides.**

## Template

**Recommended**: `how-to-guide`. The reader wants to know whether they can queue with a particular friend, why the game refused, and how to keep a stack together. What sets the guide apart is the rule turned into a table of who can play with whom, the "same division one tier away" shortcut, and the placement trap that no ranking page explains in full.

## Target keywords

Search Console has no Marvel Rivals ranked volume of its own: the feed had 1 impression, and the Championship guide's few size queries are listed above. This guide targets what a listing can't answer:

- **Primary:** marvel rivals party size, marvel rivals team size, marvel rivals ranked with friends, and marvel rivals what ranks can play together
- **Secondary:**
  - marvel rivals party restrictions, party size limit, ranked party rules, rank restrictions, 6 stack rules
  - marvel rivals rank gap, rank gap with friends, ranked difference with friends, and the game's message, "rank gap too great"
  - can you play ranked with 4 / 5 / 6 people in marvel rivals, marvel rivals 5 stack, 6 stack
  - marvel rivals placement matches with friends, placement matches rank gap, placement matches team size
  - marvel rivals pc and console ranked, can ps5 and xbox play ranked together
  - marvel rivals ranked requirements, ranked level requirement
  - how many players per team in marvel rivals
- **Leave to the feed:** "marvel rivals lfg", "team finder", "duo finder", "group finder", "6 stack discord"
- **Leave to the Championship guide:** factions, the Championship, Ignite
- **Leave out:**
  - hero tier lists, team-ups, bans and climbing tips. That's gameplay.
  - the rank list in full, rank distribution, rank rewards and the Chrono Shield. Booster sites and the wiki own them.
  - "crossplay ranked coming" speculation: NetEase has announced nothing.
- **Questions:**
  - How many players are on a team in Marvel Rivals? (People Also Ask: "Is Marvel rival 5v5 or 6v6?")
  - How many people can be in a party in Competitive? (PAA: "How many people can be in a party in Marvel Rivals?", "Can you play comp with 4 people in Marvel Rivals?")
  - Why can't I queue Competitive as a party of five?
  - What ranks can play together? Can Gold queue with Bronze, Platinum with Silver?
  - Can you play placement matches with friends?
  - Can PC and console players play ranked together? (PAA: "Can PC and console play ranked together?", "Why doesn't Marvel Rivals allow crossplay ranked?")

Measure after launch instead of guessing volumes: see Distribution.

## Search intent

The intent is informational. The reader has a friend the game won't let them queue with, or a group that wants to play Competitive together and needs to know who can.

What ranks, loaded on 2026-10-21:

- **"marvel rivals party size":** a seven-month-old Reddit thread first, then the Fandom wiki, a Facebook group, NetEase's [Season 5 notes](https://www.marvelrivals.com/gameupdate/20251112/41548_1270590.html) fourth, Steam discussions, Boosting Factory (11 Sep 2026), GameTree (10 Aug 2026), and NetEase's Season 3.5 notes, whose party caps Season 5 replaced. The AI Overview is right here: any size but five up to Grandmaster, duo from Celestial up, three in placements.
- **"marvel rivals what ranks can play together":** a 2025 Reddit thread, GameTree, ThisGenGaming (Apr 2025), Tribality, Facebook, the wiki, esports.gg (6 Oct 2026), GamesRadar (Feb 2026). **The AI Overview is wrong**: it says "Platinum through Grandmaster: capped at a maximum party size of three players", which is the Season 3.5 rule of August 2025. Season 5 lifted it in November 2025.
- **"marvel rivals ranked with friends":** a 2025 Reddit thread, Escapist (Jan 2025), GameTree, a 2025 YouTube video, Facebook, the wiki. The AI Overview is right.
- **"can you play ranked with pc and console":** Reddit, Facebook, Steam, Esports Insider (Oct 2025), Eneba, GameFAQs, Tapin (17 Sep 2026). The AI Overview is right: no.
- **"marvel rivals placement matches with friends":** Reddit, Facebook, Tapin (Sep 2026), ThisGenGaming, YouTube, Steam, NetEase's Season 5 notes. Tapin's is the best page on the subject and the AI Overview follows it.

**The competitors, and what each gets wrong:**

- **GameTree, "What Ranks Can Play Together in Marvel Rivals? Full List"** (updated 10 Aug 2026), the page that ranks for the pair questions. It cites the Season 3.5 notes and says Platinum to Grandmaster parties are capped at three, which Season 5 replaced. It sets the gap limit at Gold I, says "two divisions or fewer" is fine and "four or more" blocked, and never says what happens at three. It counts Diamond III and Platinum I as two steps apart; they're one. It cites nothing for the gap rule or placements.
- **Tapin** (Sep 2026), placements and crossplay. Accurate on placements, and gets the "less than three divisions" edge right ("Silver I and Gold I are three divisions apart, so they miss"). It doesn't cover the ordinary gap rule, party sizes by rank or the Celestial wall in the same place.
- **Escapist** (Jan 2025): "if you are above Gold you have a range of three divisions". Right in outline, from before placements and the Season 5 sizes.
- **Boosting Factory** (Sep 2026): party sizes right. A booster.
- **Players' answers contradict each other**, which is why they search: "there's no restriction at all until you get to platinum" (2026-04-16), "Everyone needs to be within one rank of each other" (2026-08-10), "they have to be within two ranks of you" (2026-09-13), "maximum 3 ranks apart (e.g. gold 3 and plat 3)" (2026-06-16).

So a page that gives the current rules from NetEase and the panel, works them out for every rank, and explains the placement rules players actually hit has something to add to every one of these searches, and corrects the page and the AI Overview that rank for the pair questions.

Say in the first paragraph who the guide is for: a player who plays Competitive with friends, or wants a stack to, and has been refused or wants to know who they can queue with.

**Players hit the rules without understanding them.** From r/marvelrivals (read only; the guide doesn't link Reddit):

- "why the hell cant a diamond 2 play with a friend thats plat 3" (2026-06-14)
- "our rank dispersion is a bit big 2 platinum 3 golds and one bronze/ now silver Yet we cant seem to be able to queue up 6man ranked together. Every I read it says as long we are bellow grandmaster we should be good" (2026-08-10)
- "we are gold 1 and silver 1 but it says 'has placement match player; rank gap too great'" (2026-05-28)
- "currently prospective silver 3 and my partner bronze 3 but it's saying the gap is too great" (2026-01-21)
- "My friend is in bronze 1 and im in gold 1 and i was just wondering whether we can queue together once he reaches silver 3 or do i have to wait for him to reach silver 1 or gold 3" (2026-04-16)
- "I'm GM 1 … I have 5 available friends to play but instead must be forced to play with just 2" (2026-03-17), and the reply: "Once you hit celestial you can only duo."
- "Since the restriction to only allow 3 or less players per party in placement games, myself and my friends haven't really touched Marvel Rivals as much since we play as a group of four" (2026-06-15)

The first wants the edge, the second the whole-party rule, the third and fourth the placement rule, the fifth the table, the sixth the Celestial wall, and the last the placement party size. The guide answers each.

## Content parameters

- **Word count:** 1,800–2,200. The join guide's Marvel Rivals H3 gains one sentence and links here.
- **Reading level:** plain and direct, like the other guides. It says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the other guides, Markdown in `Server/Guide/`.
- **H2 sections:** 5, plus an FAQ. Level 15 is the only requirement, so it gets a paragraph in the first H2 and an FAQ, not an H2 of its own.
- **Visuals:**
  - the game's cover, as in the other guides
  - one table: party size and who you can queue with, by rank (see the outline). The specs' `getByRole("table")` is strict, so there's only one table. Three columns, eight rows, checked at 375 px against `phone.spec.ts`.
  - the placement rules and the requirements as short lists, not more tables
  - no chart at launch (see Adding TeamTavern's data)
- **FAQ:** 6 questions.
- **Naming:**
  - "Marvel Rivals" in the title, H1 and first sentence, then "the game" or "Rivals" sparingly, as the Championship guide does.
  - "Competitive", as NetEase names the mode, and "ranked" where players' words are wanted, as in "play ranked with friends". Say once that they're the same.
  - Ranks as the game and the seed write them: "Platinum III", "Celestial I", "Eternity", "One Above All". A "rank" is Bronze, Silver and the rest, a "division" is III to I. The panel says "tiers" for ranks; the guide says "ranks" throughout and explains "divisions" once. The Championship guide's "Platinum 3" stays in its own text, since that's how its rules write it.
  - "Party", as the patch notes mostly say, and "stack" for a party that plays together regularly ("six-stack", "five-stack"), as players say.
  - "Platform" as the seed has it: PC, PlayStation, Xbox.
  - Roles as the seed has them, with the game's class names once: Tank (Vanguard), DPS (Duelist), Support (Strategist).

## Recommended title

**Marvel Rivals ranked with friends: party size and what ranks can play together**

Alternatives:

1. Marvel Rivals party size and rank gap: who can queue Competitive together
2. Can you play ranked with friends in Marvel Rivals? Party sizes and rank limits

H1: "Marvel Rivals team size, party size and playing ranked with friends". It carries "team size", which the title has no room for, as the Deadlock guide's H1 does.

Slug: `marvel-rivals-ranked-with-friends`, as the handover proposed. It's permanent, so it names the subject, not a season.

## Meta description

Marvel Rivals is 6v6. Competitive takes any party but five up to Grandmaster and duos above, within three divisions from Gold: who can queue with whom, and how to find them.

## TL;DR draft

> - Marvel Rivals is 6v6, so a full team is six. Competitive needs account level 15.
> - From Bronze to Grandmaster a party can be any size but five: solo, duo, three, four or six. From Celestial up, only solo or duo.
> - From Gold to Celestial, everyone in the party must be within three divisions of each other: Gold II reaches from Silver II to Platinum II. Bronze and Silver players can queue with any Bronze or Silver player.
> - While anyone in the party is in placement matches, the party is three at most, and its highest and lowest players must be less than three divisions apart.
> - PC and console never queue Competitive together. PlayStation and Xbox players can, but each platform keeps its own rank.

## Information gain

- **[UNIQUE INSIGHT] The rule, worked out for every rank.** NetEase's notes give sizes; the panel gives one line on the gap; players ask "can gold queue with bronze". The table answers it, and three things fall out of it that no page says plainly:
  - **Three divisions is the same division one rank away.** Gold II reaches Silver II and Platinum II, and nothing past them. It's the one sentence a player can apply without counting.
  - **The whole party is held to its widest gap.** Two Platinum, three Gold and a Silver can't queue as six, because the Platinum and Silver players are more than three divisions apart, even though every pair of neighbours is close. And without the Silver player they're five, which Competitive refuses too. So a group that spans more than three divisions plays as two parties until its lowest player catches up.
  - **The Celestial wall.** A six-stack that climbs together becomes duos the moment anyone reaches Celestial, and a Grandmaster player with a Celestial friend can queue only as a duo. So a stack that wants to keep playing as six stops climbing at Grandmaster, or splits.
- **[UNIQUE INSIGHT] Placements are stricter than the season after them.** At most three players, less than three divisions apart where the ordinary limit allows three, measured on the predicted rank, and matched at the highest player's rank. Two consequences no page draws out:
  - A friend new to Competitive starts placements at a predicted Silver III, so until their prediction moves they can place only with players from Bronze II to Silver I.
  - Every returning player's prediction starts three divisions below last season's final rank, so a duo that ended the season close starts close. But everyone who ended Celestial II or higher starts at Grandmaster II, and everyone from Bronze III to Silver III starts at Bronze III, so predictions can be closer than the ranks were.
- **[CORRECTION] The top results are out of date.** Google's AI Overview for "what ranks can play together", and GameTree's "Full List" that ranks beside it, cap Platinum to Grandmaster parties at three. That's the Season 3.5 rule of August 2025, which Season 5's notes replaced in November 2025. Say so once, without naming GameTree: "Some guides still say Platinum to Grandmaster parties are capped at three; Season 5 lifted that."
- **[UNIQUE INSIGHT] One rank per platform, even with one account.** Cross-progression links accounts, but NetEase's Q&A says rank is kept separately on PC, PlayStation and Xbox. So a PlayStation player who joins Xbox friends plays Competitive on their Xbox rank, which may be far from their PlayStation one.
- **[DATA] TeamTavern's posts.** Optional: see Adding TeamTavern's data.

## Content outline

### Introduction (~120 words)

- **Hook:** Marvel Rivals is 6v6, but most parties of six can't play Competitive together: the game limits party size by rank, how far apart the ranks can be, and who's still in placements. Players' own answers contradict each other, and the AI Overview for the question is a season out of date.
- **Who it's for:** a player who plays Competitive with friends, or wants a stack to play it with.
- **Promise:** how big a party can be, which ranks can queue together, the placement rules, what PC and console do to a party, and how to find a stack in range. Link the [Championship guide](/guides/marvel-rivals-championship-team) for Factions, and [every game's team format](/guides/join-an-esports-team#your-games-team-format) for other games.
- The TL;DR box goes here, as "In short", like the other game guides.

### H2: Team size and party size in Competitive

- **Answer first:** Marvel Rivals is played six against six, in Competitive as in Quick Match. In Competitive, a party from Bronze to Grandmaster can be any size except five; from Celestial up, solo or duo only. The game fills the empty places.
- Cite NetEase's [Season 5 notes](https://www.marvelrivals.com/gameupdate/20251112/41548_1270590.html) for the sizes, on the words that name them, and the game's own site ("Face off in heroic fast-paced 6v6 combat!", [marvelrivals.com](https://www.marvelrivals.com/)) for 6v6 if a source is wanted.
- **Why not five.** One sentence: NetEase gives no reason. The useful advice is: a party of five drops one to four, or adds a sixth.
- **The strictest rule wins.** A party's size is set by its highest player: one Celestial in a Grandmaster party makes it a duo.
- **Level 15.** Competitive opens at account level 15, by the [Season 2 notes](https://www.marvelrivals.com/gameupdate/20250401/41548_1223996.html), for every player in the party. So a friend new to the game has some Quick Match to play before they can join, and then placements at a predicted Silver III (next H2s).
- **Out of range:** Quick Match, Arcade and custom games have no party or rank limits. One sentence.
- **Some pages are out of date.** One sentence: Season 3.5 capped Platinum to Grandmaster parties at three, and Season 5 lifted it, so guides and summaries that still say three are a season behind.
- Game cover beside the first paragraph, as in the other guides.

### H2: Which ranks can queue together

- **Answer first:** from Gold to Celestial, everyone in the party must be within three divisions of each other. Every rank has three divisions, III up to I, so three divisions is the same division one rank away: Gold II can queue with anyone from Silver II to Platinum II. Bronze and Silver players can queue with any Bronze or Silver player.
- State the panel's rule as what the game shows, uncited: "Gold to Celestial tiers can team within a maximum range of three divisions."
- **Table: party size and who you can queue with.** Columns: Your rank / Party size / Who you can queue with. In the game's order:

  | Your rank | Party size | Who you can queue with |
  |---|---|---|
  | Bronze and Silver | Any but five | Any Bronze or Silver player, and Gold up to your division: Bronze II or Silver II reaches Gold II |
  | Gold | Any but five | Three divisions either way, counting Bronze as Silver: Gold II reaches Silver II, Bronze II and Platinum II |
  | Platinum | Any but five | Gold to Diamond, three divisions either way |
  | Diamond | Any but five | Platinum to Grandmaster, three divisions either way |
  | Grandmaster | Any but five, a duo with a Celestial | Diamond to Celestial, three divisions either way |
  | Celestial | Solo or duo | Grandmaster to Celestial, three divisions either way; Eternity and One Above All from Celestial II, within 200 points |
  | Eternity and One Above All | Solo or duo | Each other, and Celestial II or I within 200 points |

  How it's derived, for the writer: number the divisions Bronze III = 1 to Celestial I = 21, and count a Bronze division as the Silver division of the same number (Bronze II as Silver II). Outside placements, a pair where either player is Gold III (7) or higher is allowed at a gap of 3 or less; a pair of Bronze and Silver players at any gap. A party is allowed when every pair is.
- One or two sentences under the table: the party is held to its widest gap, so a six-stack fits only if its highest and lowest players are within three divisions. Use the reported case without its source: two Platinum, three Gold and a Silver can't queue as six, since Platinum and Silver are at least four divisions apart, and without the Silver player they're five, which is refused. So they play as two parties, say the Platinum and Gold players as four or the Golds and the Silver as four if the Golds are within three of the Silver, until the Silver player catches up.
- **The message.** When a party is too far apart the game refuses the queue. The 2025 text, "Team member RANK gap is significant.", has no 2026 screenshot, so don't quote it here; say the game refuses the queue and names the rank gap. (The Championship guide quotes it; see "Not confirmed".)
- **Eternity and One Above All.** One or two sentences: they queue solo or as a duo, and can team with a Celestial II or I player within 200 points, as the panel says.

### H2: Placements: three players, two divisions

- **Answer first:** the first 10 Competitive matches of a season are placement matches. While anyone in the party is still playing theirs, the party can be three players at most, and its highest and lowest players must be less than three divisions apart, so at most two. A higher rank's stricter rule still applies.
- Cite the [Season 5 notes](https://www.marvelrivals.com/gameupdate/20251112/41548_1270590.html), which say: "While any player is in placement matches, they may only queue in teams of up to three players. For teams that include at least one player in placement matches, the difference between the highest-ranked and lowest-ranked player in the party must be less than 3 divisions."
- Cover:
  - **The message:** "Has placement match player, rank gap too great". Stated as what the game shows, uncited. Reported word for word on 2026-05-28 and 2026-09-17.
  - **Predicted rank.** During placements each player has a predicted rank, which the game uses for matchmaking and for who can party. It moves with every win and loss, so a pair that fits after one match may not after the next.
  - **Where predictions start:** last season's final rank less three divisions; Celestial II and higher start at Grandmaster II; Bronze III to Silver III start at Bronze III; a player who has never finished 10 Competitive matches in a season starts at Silver III. So a new friend can place only with players from Bronze II to Silver I at first.
  - **Matched at the top.** A party with anyone in placements is matched at its highest player's rank (Season 5 notes: "the highest-ranked player in the team as the search anchor"). So a placement player in a higher friend's party plays that friend's lobbies.
  - **Your own play counts.** Since the [Season 7 notes](https://www.marvelrivals.com/gameupdate/20260318/41548_1291772.html), placements adjust the result by up to 100 points either way on your own performance, not your team's.
  - **Placements last the season.** A player who finishes them in the first half keeps the rank at the mid-season update; one who doesn't can finish them in the second half (Season 5 notes). One sentence: the .5 update doesn't start placements again.
- **What to do:** a group of four or more plays placements in a three and a single, or one at a time, and regroups once everyone has a rank.

### H2: PC, PlayStation and Xbox

- **Answer first:** PC and console players never queue Competitive together. PlayStation and Xbox players can, and Quick Match and custom games take everyone.
- Cover, citing NetEase:
  - PC and console are matched apart in Competitive: the [closed beta FAQ](https://www.marvelrivals.com/news/official/20240719/40185_1168280.html) first said so ("the competitive mode does not support matching PC users with console users"), and the [cross-progression Q&A](https://www.marvelrivals.com/announcements/20250924/40955_1261246.html) says the game "matches players by device".
  - **Rank per platform.** The Q&A: rank and leaderboard "are independent on each platform", even with linked accounts. So a party's gap is measured on the rank each player has on the platform they're playing on. A PlayStation player who joins Xbox friends brings their Xbox rank, and a player new to a platform does placements there.
  - PlayStation and Xbox share one Competitive pool, from players' reports; state it as what happens, uncited. PS4 is included since September 2025.
  - **No ranked crossplay is announced.** One sentence: NetEase hasn't said PC and console will share Competitive. Don't speculate.
- **What to do:** a mixed PC and console group plays Quick Match together, and splits by platform for Competitive.

### H2: Finding a stack in range

- **Answer first:** recruit players within three divisions of everyone already in the stack, on the same platform (or PlayStation and Xbox together), at the hours you play. Below Celestial, a stack of six; at Celestial and above, a duo.
- Cover:
  - **Range first.** Use the table, and leave room: ranks move during a season, so a stack with everyone in one rank keeps fitting when a gap of exactly three doesn't.
  - **Six, not five.** Recruit to four or six. A stack of five that loses a player one night still queues; one of five never does.
  - **Roles.** There's no role queue in Competitive, so a stack picks its composition. Name the roles you're short of, Tank (Vanguard), DPS (Duelist) or Support (Strategist), not all three.
  - **Server.** Agree on one region's servers, as the Championship guide says; link it rather than repeating the list.
  - **Keep a good random.** After a good match, add them by their Marvel Rivals username, the same on every platform.
  - **From ranked to the Championship.** A stack that plays together every week can enter the Marvel Rivals Championship as a Faction, once every member is Platinum III that season. Link the [Championship guide](/guides/marvel-rivals-championship-team) rather than repeating it.
  - **The official Discord.** The [official Marvel Rivals Discord](https://discord.gg/marvelrivals) has LFG channels. One sentence.
  - One line linking the make guide for keeping a team together: "How to make an esports team".
- **The TeamTavern step:** put up a player post with your rank, roles, server and platform, and "Ranked" ticked under Looking for. A stack looking for players puts up a group post with a rank range that the whole stack can queue with, within three divisions of its members, not "Bronze to One Above All". Then filter the posts by rank and platform and message the ones that fit. Link `/games/marvel-rivals` with the anchor "Marvel Rivals players and groups on TeamTavern".

### FAQ (6)

1. **How many players are on a team in Marvel Rivals?** Six. Matches are 6v6, and a Competitive party from Bronze to Grandmaster can be all six.
2. **Why can't I queue Competitive as a party of five?** Five is the one party size Competitive refuses from Bronze to Grandmaster. Queue as four, or add a sixth. From Celestial up, it's solo or duo only.
3. **What ranks can play together in Marvel Rivals?** From Gold to Celestial, players within three divisions of each other: the same division one rank up or down is the edge. Bronze and Silver players can queue with any Bronze or Silver player.
4. **Can you play placement matches with friends?** Yes, as a party of up to three whose highest and lowest players are less than three divisions apart, on predicted rank.
5. **Can PC and console players play ranked together?** No. PlayStation and Xbox players can, though each platform keeps its own rank.
6. **What level do you need for Competitive?** Account level 15.

### Conclusion (~100 words)

- Takeaways: four or six, never five; keep the stack within three divisions, and inside one rank to be safe; play placements in threes; split by platform for Competitive; at Celestial, it's duos.
- **Call to action:** "Marvel Rivals players and groups on TeamTavern".

## Facts, with sources

Checked 2026-10-21.

**NetEase's own pages. Cite these:**

- **Party sizes, placements and their party rules**, from the [Season 5 notes](https://www.marvelrivals.com/gameupdate/20251112/41548_1270590.html), 12 Nov 2025:
  - "Competitive Team Limit Adjustments: 1. Bronze to Grandmaster: Team up with any party size except five-stack. 2. Celestial/Eternity/One Above All: Only solo or duo queue allowed."
  - "From Season 5 onward, your first 10 ranked matches are Placement Matches."
  - "During placement matches, you will receive a predicted rank reflecting your estimated skill level. This predicted rank is used for matchmaking and team formation during placement matches."
  - "If you complete placement matches in the first half of the season, you'll keep that rank in the second half. If you don't finish placements in the first half, you must complete them in the second half to be eligible for ranked rewards."
  - Subsequent placements: "future initial estimated ranks are based on your last season's final rank minus 3 sub-tiers. … players ranked in the Celestial II, Celestial I, Eternity, and One Above All will be uniformly reset to Grandmaster II … Players in Bronze III, Bronze II, Bronze I, and Silver III will start at Bronze III … If you have never played 10 ranked matches in a season, you are considered a new player and will start at Silver III estimated rank."
  - "You can be placed up to 1–2 divisions above your previous season's final rank, but you will not be placed higher than Celestial I." Not for the guide.
  - "While any player is in placement matches, they may only queue in teams of up to three players. For teams that include at least one player in placement matches, the difference between the highest-ranked and lowest-ranked player in the party must be less than 3 divisions. If stricter party limits apply (for example, if the party contains players at Celestial/Eternity/One Above All tiers), those stri[cter limits apply]"
  - "If a team contains any player in placement matches, the matchmaking will use the highest-ranked player in the team as the search anchor when finding teammates and opponents."
  - Penalties for a stack member who disconnects during someone's placement: a loss for the placement player, no suspension. Not for the guide.
- **The rule Season 5 replaced**, from the [Season 3.5 notes](https://www.marvelrivals.com/gameupdate/20250801/41548_1251405.html), 1 Aug 2025: "1. Players in Bronze, Silver, and Gold can queue for Competitive in any team size except 5-player teams. 2. Players in Platinum, Diamond, and Grandmaster can queue for Competitive in teams of up to 3 players. 3. Players in Celestial, Eternity, and One Above All can queue for Competitive in teams of up to 2 players." For knowing which pages are out of date. Don't link it from the guide.
- **Placements by your own play**, from the [Season 7 notes](https://www.marvelrivals.com/gameupdate/20260318/41548_1291772.html), 18 Mar 2026: "Every placement match now tracks your individual performance (not tied to teammates). After ten placement matches, your rank will be adjusted between -100 and +100 competitive points based on your personal results and tier."
- **Level 15 and Eternity duos**, from the [Season 2 notes](https://www.marvelrivals.com/gameupdate/20250401/41548_1223996.html), Apr 2025: "The minimum level required for new players to participate in Competitive has been adjusted to 15. (If you have previously completed at least one valid competitive match, you can still join Competitive in Season 2.0 even if you are below level 15.)"; "When a team includes players from Eternity or One Above All, only solo or duo queuing will be allowed."
- **Rank per platform**, from the [cross-progression Q&A](https://www.marvelrivals.com/announcements/20250924/40955_1261246.html), 24 Sep 2025: "because Marvel Rivals matches players by device, your battle data, rank, and leaderboard on PC, PlayStation, and Xbox are saved separately under your Career for each device. Even though you have just one character, your rank and leaderboard are independent on each platform."; "your PlayStation rank and standing will not carry over to your PC rank and standing."
- **PC and console apart in Competitive**, from the [closed beta FAQ](https://www.marvelrivals.com/news/official/20240719/40185_1168280.html), 19 Jul 2024: "Players can engage in cross-platform matchmaking, but the competitive mode does not support matching PC users with console users." Old, but the Q&A's "matches players by device" and the separate PC and Console Championship events agree with it.
- **6v6**, from [marvelrivals.com](https://www.marvelrivals.com/): "Face off in heroic fast-paced 6v6 combat!" The [Steam page](https://store.steampowered.com/app/2767030/) says "Fast and Fun 6v6 Action".
- **No change since.** Every patch note from 3 Dec 2025 to the 15 Oct 2026 patch was searched: no change to party sizes, the gap rule, placement parties or crossplay. Season 6 (14 Jan 2026) only repeats the Championship's Platinum 3. Season 10.5 launched 9 Oct 2026 and runs to Season 11, expected in mid-November.
- **The official Discord**, [discord.gg/marvelrivals](https://discord.gg/marvelrivals), linked from the Championship guide: "Find friends to play with in our LFG channels!"

**From the client, through players' reports.** Enough to state as what happens in the game, never cited or quoted except the panel and message texts:

- **The "RANK RESTRICTIONS" panel**, screenshot of 2026-01-11 ([image](https://i.redd.it/vhp5h3nvnncg1.png)):
  1. "Gold to Celestial tiers can team within a maximum range of three divisions."
  2. "Eternity and One Above All players can team with Celestial II players within 200 points, but if the difference exceeds 200 points, it must be less than 200."
  3. "Players from Bronze to Grandmaster can form teams of any size except five players."
  4. "Above Celestial, players may only join Competitive as solos or duos."

  Item 4 says "above Celestial"; Season 5's notes say "Celestial/Eternity/One Above All". Go with the notes.
- **Three divisions apart is allowed outside placements, four isn't:** Diamond II with Platinum III, four apart, refused ([2026-06-14](https://old.reddit.com/r/marvelrivals/comments/1u5uhw1/competitive_mode_rank_gaps_suck/)); "Plat 2 can queue with players from gold 2 to diamond 2" and "a gold 3 can play with plat 3 AND silver 3, but silvers can't play with plats" ([2026-08-10](https://old.reddit.com/r/marvelrivals/comments/1vkj6sh/marvel_rivals_ranked_for_6_stack/)); "maximum 3 ranks apart (e.g. gold 3 and plat 3)" ([2026-06-16](https://old.reddit.com/r/marvelrivals/comments/1u7ifz5/question_about_ranked_queueing/)). These are answers, not tests, but they agree with the panel's "maximum range of three".
- **A six-stack across Platinum, Gold and Silver is refused** ([2026-08-10](https://old.reddit.com/r/marvelrivals/comments/1vkj6sh/marvel_rivals_ranked_for_6_stack/)).
- **Three divisions apart is refused in placements:** Gold I with Silver I ([2026-05-28](https://old.reddit.com/r/marvelrivals/comments/1tpqfy2/why_cant_i_queue_with_my_friend_in_comp/)), predicted Silver III with Bronze III ([2026-01-21](https://old.reddit.com/r/marvelrivals/comments/1qizjpd/placement_matches_and_rank_gap_too_great/)), Bronze II with one Silver II but not another ([2026-09-17](https://old.reddit.com/r/marvelrivals/comments/1wih511/cant_play_ranked_with_friend_despite_being_within/)).
- **The placement message**, "Has placement match player, rank gap too great": 2026-05-28 and 2026-09-17.
- **A four-player group splits in placements** ([2026-06-15](https://old.reddit.com/r/marvelrivals/comments/1u6wq0d/anybody_else_strayed_away_from_the_game_due_to/)).
- **Grandmaster and Celestial friends split to duos** ([2026-03-17](https://old.reddit.com/r/marvelrivals/comments/1rvtgqh/not_being_able_to_play_comp_with_a_full_stack/)).
- **PlayStation and Xbox share Competitive**, from several players in Sep 2025, May 2026 and Sep 2026 (see the Championship brief), and a mixed PlayStation and Xbox party in placements on 2026-09-13.

**Not confirmed, or sources disagree.** The guide handles each like this:

- **Bronze with Gold. Settled while writing:** players report that the game counts Bronze and Silver as one rank for parties outside placements, so a Bronze II is as close to a Gold player as a Silver II. "When I was gold 3 I could queue with my bronze 2 friend, but now that I'm plat 3 I cant queue with my silver 1 friend", answered "Bronze and Silver specifically states that they're treated as the same rank for partying" ([2024-12 thread](https://old.reddit.com/r/marvelrivals/comments/1hhgk8t/party_rank_difference_plat_2_cant_queue_with_gold/), replies of 2025-01); "A bronze2 player can queue alongside a gold2 player" ([2025-07-15](https://old.reddit.com/r/marvelrivals/comments/1m0kfc0/if_i_cant_play_with_my_friends_who_are_2_ranks/)); "Bronze/Silver is treated as a a single rank, so a Bronze 2 can queue with a Gold 2. The only exception to this rule is for Placement Matches" ([2026-03-30](https://old.reddit.com/r/marvelrivals/comments/1s7agdj/they_really_need_to_fix_the_rank_disparity_system/)). That agrees with the panel, and the guide's table merges Bronze and Silver into one row. In placements they count separately: Silver I with Bronze II (2026-03-29) and Silver III with Bronze III (2026-01-21) are refused.
- **Divisions or points.** Two replies say the placement limit is measured in points ("within 300 points", ">300 point difference"), as the Eternity rule is. With 100 points a division, a pair "two divisions apart" by name can be more than 300 points apart at the edges, which would explain the Bronze II with one Silver II but not another. The guide counts divisions, as NetEase and the panel do, and says once that a pair at the edge of the limit may be refused by a few points. Don't give a point figure.
- **The non-placement message.** "2132: Team member RANK gap is significant." is from a 2025 screenshot; players in 2026 paraphrase it ("the rank gap is too large", "rank gap too wide", "too much of a rank difference"). The guide doesn't quote it. The Championship guide does; if it keeps the H2, soften it the same way.
- **Eternity with One Above All.** No source covers it directly; a 2025 report has two One Above All players refused. The table's last row says "each other" from the duo rule. Leave it as is unless a report contradicts it, and keep the sentence short.
- **How a mixed party is matched outside placements.** NetEase states the highest-player anchor only for placements. Don't claim it for ordinary Competitive.
- **Season 11.** Expected mid-November 2026; NetEase gives no date yet. Check its notes before or just after launch.

## Adding TeamTavern's data

**Counted while writing**, from the 2026-10-21 production backup restored into a throwaway container (`%LOCALAPPDATA%/Temp/gsc/rivals-posts.sql`): 34 player, 3 group and 1 community post, all from 2026-09-30 on. 27 of the 34 player posts (79%) tick Ranked. Player posts by platform: PC 21, PlayStation 11, Xbox 2. All three group posts ask for a range one party can't queue across: Platinum III–Diamond II (four divisions), Grandmaster II–Eternity (Eternity takes only Celestial II and up), Bronze III–One Above All. The guide uses the Ranked share and the three group ranges, with their counts.

The plan before the count:

- player and group posts, and the share ticking "Ranked" under Looking for
- player posts by rank and by platform
- of group posts with a rank range, how many have ends more than three divisions apart, which a stack can't queue across

If there are fewer than about 30 posts, use none of it: a share of a dozen posts says nothing. At launch the guide has no chart either way.

## Internal links

- **Links from this guide:**
  1. `/games/marvel-rivals`, twice: in the stack H2 and in the conclusion. Anchor: "Marvel Rivals players and groups on TeamTavern".
  2. The Championship guide, from the introduction and the stack H2. Anchor: "the Marvel Rivals Championship" or "how to build a Marvel Rivals Championship team".
  3. The make guide, from the stack H2. Anchor: "How to make an esports team".
  4. The join guide, from the introduction ("every game's team format").
- **Links to this guide:**
  1. The Championship guide, as settled under Overlap: its rank H2 links here with "who can queue Competitive together".
  2. The join guide: in its "Marvel Rivals: the Championship" H3, one sentence: Competitive takes parties of any size but five up to Grandmaster, within three divisions from Gold, and a link here with the anchor "playing ranked with friends in Marvel Rivals".
  3. The make guide: a row in its format table, beside the other ranked rows: **Marvel Rivals: Competitive** | Nobody: a party of up to 4, or 6, queues together below Celestial, and a duo above | Everyone within three divisions from Gold; in placements, three players at most, less than three divisions apart.
  4. The `/guides` index, and the guide's `games` list naming `marvel-rivals`, so it shows on the guides list with its game.
- Editing the join, make and Championship guides adds or moves a fact, so their `updated` changes too.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, and logo-512.png as `image`, as the other guides have. BreadcrumbList Home > Guides > the H1. `datePublished` and `dateModified`. No Person.
- Every rule the guide cites links to NetEase's own page, on the words that name it, once per source per section. What comes from the client and players' reports (the panel, the placement message, PlayStation and Xbox sharing a pool) is stated as what the game shows, without a link. Reddit and community Discord servers are neither linked nor named; the official Marvel Rivals Discord can be.
- No "when we played" stories. Nobody here has the game.

## Keeping it current

A Marvel Rivals season lasts about two months, with a .5 update in the middle. Party rules changed in Season 2, Season 3.5, Season 5 and Season 7:

- **Season 11, expected mid-November 2026.** Read its notes for "Competitive", "Team Limit", "placement" and "party", with `grep-notes.mjs`. A new season also resets ranks, so the start-of-season placement questions peak then.
- **Each .5 update and weekly patch**, with the same script.
- **The panel's wording**, if a newer screenshot turns up.

Change `updated` only when a fact changes.

## Implementation

- `Server/Guide/marvel-rivals-ranked-with-friends.md`, imported as text in `Guides.js`, and an entry in `Guides.purs`: slug, heading, title, description, published, updated, and `games: [ { handle: "marvel-rivals", title: "Marvel Rivals" } ]`.
- The sentence in the join guide's Marvel Rivals H3, the row in the make guide's table, and the Championship guide's H2 and FAQs as settled under Overlap.
- The cover is already at `/images/games/400/marvel-rivals.webp`.
- A test in `test-playwright/integration/guides.spec.ts` (join guide → this guide → feed), and the slug in the lists in `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and `test-playwright/screenshots.mjs`. The table at 375 px has the longest cells of any guide's (the Celestial row); check it in `phone.spec.ts` and the 375 px screenshot.
- Verify with `.\bundle.ps1 -environment local`, then `npm test -- guides.spec.ts crawlers.spec.ts accessibility.spec.ts phone.spec.ts --timeout=120000`, then fetch the page as Googlebot from http://localhost:8020 and check the title, H1 and description.

## Timing

1. Write the guide now and launch it as soon as it's checked, with `published` and `updated` set to the real launch day. Launching three to four weeks before Season 11 lets Google index it before the start-of-season placement questions.
2. When Season 11's notes are out, check the guide against them, update it where a fact changed, change its `updated`, and request indexing again.

## Distribution

The same plan as the other guides:

- request indexing in Search Console on publish
- link it from the join, make and Championship guides and the guides index
- after 4–6 weeks, check which Marvel Rivals queries reach this guide and which still reach the Championship guide. If "party size" or "6 stack" queries stay on the Championship guide, its overlapping text is competing; if a listing query ("lfg", "duo finder") lands here, the guide is competing with the feed, so adjust its title.
