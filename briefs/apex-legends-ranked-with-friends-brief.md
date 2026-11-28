# Content Brief: Apex Legends party size and playing ranked with friends

The sixteenth game guide, and the first for Apex Legends. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the other ranked briefs. It covers what decides whether friends can queue Apex's Ranked together:

- squad size: Ranked is trios, a premade is two or three, and the game fills the rest
- what a player needs first: Account Level 20
- the party rule: every premade squadmate within two tiers, and what that allows from each tier
- how the game matches a premade: by its highest player's RP, and what that does to entry cost
- the 2026 tests (one tier, solo queue for Diamond and up) and that neither was kept
- separate ranks per platform, crossplay, and a console player in a PC player's party
- splits and resets, which move a squad's ranks apart and back
- partying with cheaters

Apex Legends is already on the site, so the guide goes out once it's written, and the join guide then links here.

Prepared 2026-11-28. Evidence comes from:

- Google autocomplete for 15 Apex seeds, fetched from `suggestqueries.google.com` with `hl=en&gl=us`
- EA Help's [How Apex Legends Ranked works](https://help.ea.com/en/articles/apex-legends/ranked/) ("Updated 3 weeks ago" on 2026-11-28, so early November), [game modes](https://help.ea.com/en/articles/apex-legends/game-modes/), [cross-play](https://help.ea.com/en/articles/apex-legends/add-friends-cross-play/), [Cross Progression](https://help.ea.com/en/articles/apex-legends/cross-progression/) and [play by the rules](https://help.ea.com/en/articles/apex-legends/play-by-the-rules/), read with curl on 2026-11-28 (WebFetch got 503s from help.ea.com; curl with a browser user agent got 200s)
- EA's news posts on Ranked and matchmaking from November 2025 to November 2026 (list under Facts), read in full with curl and every quote below checked against the full text. Chromium gets a 502 from ea.com through this environment's proxy, while curl with a browser user agent gets the page. forums.ea.com puts up a bot-verification page even in Chromium, but every forum post the brief needs also has an ea.com copy.
- the top search results for "apex legends ranked party restrictions rank difference", for what other pages say
- GUIDES.md was not available to this session, so there is no Search Console evidence for Apex here. Check it before the guide is written.

## Why this topic

- **The pattern that works.** Guides that answer how a game's ranked mode treats friends rank at 4–8 within days.
- **Autocomplete asks exactly this.** "apex rank difference" gives ten suggestions, among them "party", "queue", "tier difference", "max rank difference", "2 rank difference" and "max 2 rank difference". "apex ranked party" gives "restrictions", "rules" and "difference"; "apex ranked with friends" gives "cant play ranked with friends" and "who can i play with"; "apex ranked duo" gives ten, from "is there apex ranked duos" to "did apex remove ranked duos". "apex ranked solo queue" gives "test", "end date" and "diamond", and "can you play apex ranked with" gives "pc and console" and "crossplay".
- **The ranking pages disagree, and most are out of date.** What they say for the party limit:
  - one tier (pley.gg, May 2026), which was a test EA ended
  - three ranks (an older copy of EA's own help page, still in results)
  - three tiers (a Dot Esports article)
  - one tier from Platinum (EA's Ranked Arenas post, for a mode Apex has removed)
  - Masters and Predators limited to Diamond, which describes 2025's matchmaking tests, not a party rule

  EA's help page, updated this month, says two tiers and gives an example. EA's 30 September post says neither 2026 test became permanent. No ranking page puts those two together.
- **The year was confusing for players.** EA tested a one-tier limit and solo queue only for Diamond and up in Season 29, and dropped both on 30 September. A player who stopped in the summer may still think either one applies.
- **A detail other pages miss:** a premade's RP counts as its highest player's for entry cost and Challenger or Goliath matches (Cipher patch notes). So a lower-ranked friend pays the higher player's stakes.
- **Apex is a feed without a guide.** It had 7 active posts on 9 October (SUBREDDITS.md), and it's the biggest game in the catalogue with no guide of its own.

**Why not the ALGS.** The Challenger Circuit is a separate guide, best written once Year 7's rules are out, after the Championship in Sapporo (28–31 January 2027). This guide links the join guide's Apex section for it.

## Template

**Recommended**: `how-to-guide`, as the other ranked guides. The reader has just failed to queue with a friend, or wants to know before they try. What sets the guide apart:

- the two-tier rule, from EA's current help page, with a table of who each tier can queue with
- the 2026 tests, dated, and that neither stands
- what queueing with a higher friend costs: the highest player's RP for matchmaking and entry cost
- separate ranks per platform, which decide which of a friend's two ranks counts

## Target keywords

- **Primary:** apex ranked with friends, apex rank difference party, apex ranked party restrictions, apex max rank difference
- **Secondary:**
  - apex rank tier difference, apex 2 rank difference, apex max 2 rank difference, apex rank difference queue
  - apex ranked party rules, apex ranked party difference, apex ranked restrictions
  - apex cant play ranked with friends, can you play apex ranked with friends, apex ranked who can i play with
  - is there apex ranked duos, can you play apex ranked duos, did apex remove ranked duos
  - apex ranked solo queue, apex ranked solo queue test, apex ranked solo queue end date
  - apex ranked level requirement, what level to play apex ranked
  - can you play ranked apex with pc and console, apex ranked crossplay
  - apex ranked entry cost

## Search intent

Mostly a player whose matchmaking just failed with a friend, or who's about to try: can we queue together, and if not, why? Second, a player who heard about solo queue for Diamond and up and wants to know whether it's still on. Third, a player on console whose friend plays on PC. The guide answers each in its first lines and in the FAQ, then sends the reader to find players within two tiers.

## Recommended title

- **heading (H1):** Apex Legends squad size, party size and playing ranked with friends
- **title:** Apex ranked with friends: rank difference, party size and solo queue
- **slug:** `apex-legends-ranked-with-friends`. It matches the sibling ranked guides, and it still fits after Season 32 changes the ranks.

## Meta description

Apex ranked is trios. A premade squad must be within two tiers, plays at its best player's RP, and can queue at Diamond and above again: who you can play with.

(The final text gets checked for length against the siblings in `Guides.purs`.)

## TL;DR draft (In short)

- Ranked is played in squads of three. You can queue alone or as a premade of two or three, and the game fills the empty places. EA's list of modes has no Ranked Duos.
- Every player in a premade must be within two tiers of every other. A Bronze player can queue with Rookie, Bronze, Silver and Gold, but not Platinum or higher.
- A premade is matched, and pays its entry cost, at its highest player's RP. A Gold player queueing with a Diamond friend plays and pays as Diamond.
- In 2026, EA tested a one-tier limit and solo queue only for Diamond and above. On 30 September it said neither would be kept, so in Season 31 premades can queue at every rank.
- Your rank is separate on each platform. Ranked needs Account Level 20.

## Information gain

- The rule quoted from EA's current help page, with its example, turned into a full table: from each tier, who you can queue with. Rookie to Master, with Predator as the top of Master.
- The 2026 tests, dated from EA's own posts: one tier and Diamond-and-up solo queue in Season 29 (8 June post), dropped on 30 September ("we are not making the one-tier restriction permanent"; "We are not rolling out the solo queue experiment as a permanent requirement").
- From the Cipher patch notes: "all premade squadmates' RPs are treated as equal to the value of the highest squadmate's RP", for Squad Tier, which decides Challenger and Goliath matches. A non-premade teammate is counted at their own RP.
- Ranks are kept per platform (EA Help), so a friend who plays on both PC and console has two ranks, and only the platform's rank they're playing on matters. The last part is inferred, hedged.
- The split reset table, which shows how two friends a tier apart at the end of split one can land a tier closer: Diamond II and I both reset to Platinum II.
- What Season 32 changes (Iron, Emerald and Grandmaster, an Open League and a Pro-Am League), and that EA hasn't said how it affects premades.

## Content outline

### Introduction (~110 words)

Lead with the answer: a premade squad in Apex Ranked must be within two tiers, so Gold can queue with Silver to Diamond, and it plays at its best player's RP. EA's 2026 tests of a one-tier limit and of solo queue for Diamond and up are over. Then "This guide is for …", "It covers …", and the link to every game's team format.

### In short

As above.

### H2: Squad size and who can play Ranked

Cover image. Ranked is Core Apex: squads of three, 20 squads to a match (EA Help's scoring section: "out of the 20 teams"). Queue alone, or as a premade of two or three, and the game fills the empty places with players who queued alone. EA's list of modes has no Ranked Duos: Duos exists only as an unranked mode. Every player needs Account Level 20. The Ranked Roadmap (20 October) removed the Road to Ranked challenges and moved access "back to only requiring Account Level 20", and says EA may still change the level.

### H2: How far apart a premade can be

EA's rule, quoted: "All players in a premade Ranked squad have to be within two ranks of each other. If they aren't, matchmaking will fail." "Ranks" here means tiers, as EA's own example shows: "A Bronze player can play with Rookie, Bronze, Silver, and Gold players, but can't play with anyone ranked Platinum or higher." Divisions don't matter: Gold IV and Diamond I are two tiers apart.

The guide's one table: each tier, the tiers it can queue with.

| Your tier | Can queue with |
|---|---|
| Rookie | Rookie, Bronze, Silver |
| Bronze | Rookie to Gold |
| Silver | Rookie to Platinum |
| Gold | Silver to Diamond |
| Platinum | Gold to Master |
| Diamond | Platinum to Master and Predator |
| Master and Predator | Platinum to Master and Predator |

The Bronze row is EA's own example; the rest applies the same rule. Predator is a title within Master, "the top 750 players per platform in the Master rank", so the guide treats it as Master. Hedge that EA's example covers only Bronze. In a trio the rule applies to every pair, so the lowest and highest must be within two tiers.

Then the tests: in Season 29 EA tested a one-tier limit ("we limited squad-ups to 1 tier above or below your rank, instead of 2") and solo queue for Diamond and above ("If you're Diamond or higher, you won't be able to squad up as a premade in Ranked"), from its 8 June post. On 30 September it said it wasn't making either one permanent. The post names the standing rule too: the one-tier limit wasn't worth it for "everyone else who was used to playing with friends within two rank tiers from them". It also says "That does not mean solo queue experiments are dead", and that what it carries forward is the aim "without taking away your ability to play with your friends". The help page warns that "Matchmaking rules can change during temporary tests", so the guide says to check the latest patch notes when a queue fails unexpectedly.

### H2: Your premade plays at its best player's rank

The help page: "You'll be matched against other squads using the highest RP value in your premade squad." The Cipher patch notes add the cost: Squad Tier is the average of the squad's RP, but premade squadmates count as the highest one's RP. A non-premade teammate counts at their own. Squad Tier against the lobby's median decides the match type. Two or more tiers below the median is a Challenger match, which costs 30% less. Two or more tiers above is a Goliath match, which costs 50% more. Entry costs from the help page: Rookie 0, Bronze 10, Silver 20, Gold 38, Platinum 48, Diamond 65, Master and Predator 90 RP. Each player still scores their own RP from placement, kills, assists and participations, so a lower friend can still climb, but plays harder lobbies to do it. Demotion protection's three shields apply to each player.

### H2: Splits, resets and staying within two tiers

Ranks reset twice a season, at its start and at split two. At a season's start each player is placed by their hidden Ranked skill, so two friends who finished together can start further apart. The split two reset table, from the help page, brings Diamond and Master down into Platinum, which can bring a squad back within two tiers. Season 31 began on 3 November. EA hasn't published split two's date; it shouldn't be guessed.

Season 32, "in early 2027", replaces Rookie with Iron, "treated like a standard rank for all players", adds Emerald between Platinum and Diamond as "the new threshold between Open and Pro-Am Leagues", and adds Grandmaster at the top, with Predator going to the top 750 of Grandmaster at the season's end. The roadmap says nothing about premades, and calls itself not "everything". If two tiers still holds, Emerald means a Platinum player reaches Diamond but no longer Master, but the guide doesn't say so before EA does. It says Season 32 changes the ranks, that EA hasn't said what it does to premades, and that the guide will be updated then.

### H2: Platforms and crossplay

- Ranked progress is kept per platform: "you could be Bronze rank on one platform but Gold on another"; PS4 and PS5 count as one platform. Two friends compare the ranks on the platforms they'll play on.
- Crossplay is on by default on every platform. A console player can turn it off, and then plays only with players on the same platform who have also turned it off. A PC player can't turn it off.
- A console player who parties with a PC player plays in PC lobbies. The only source found is a Respawn engineer's TikTok in 2020, before crossplay launched, quoted by [GamesRadar](https://www.gamesradar.com/you-will-only-be-in-pc-crossplay-lobbies-if-you-have-a-pc-player-on-your-team-says-apex-legends-dev): "You will only be in PC crossplay lobbies if you have a PC player on your team." EA's Season 6 post that search results cite now answers 404. Without a current EA source, the guide gives it as how Respawn described crossplay at launch, or leaves it out.
- Ranked Ladders need crossplay on.

### H2: Who you queue with matters

EA's rules: "Intentionally partying up with cheaters in matches to boost your rank … counts as cheating", and partying with confirmed cheaters "for multiple matches in a period of time" can get you banned. Queue with people you know. This answers the boosting questions the two-tier rule exists for: EA says the rule is there "to prevent boosting".

### H2: Finding a squad in range

Recruit within two tiers of everyone, with a tier to spare, since a reset or a bad week moves ranks. Roles: the seed's Fragger and Support, and whether you can lead. Say which you play. Platform: a squad of console players who want console lobbies has to be all on console. **Post on TeamTavern:** pick your "Rank" (divisions included, "Gold II"), your "Role", "In-game leader" if you call, your "Platform", "Ranked" under "Looking for", and your EA ID, plus Discord, Steam, PSN, Xbox gamertag or Nintendo friend code as fits. A group post gives a rank range, and a range more than two tiers wide can't queue as one premade. Feed link: `[Apex Legends players and groups on TeamTavern](/games/apex-legends)`. Post counts from the production copy go here if they say something (see Adding TeamTavern's data).

### FAQ (6)

- **What is the rank difference limit in Apex ranked?** Two tiers, from every squadmate to every other, whatever the division.
- **Can Diamond players play ranked with friends in Apex?** Yes. Solo queue for Diamond and above was a two-week test in Season 29, and EA said on 30 September it wasn't keeping it.
- **Is there ranked duos in Apex?** No. Ranked is trios. A duo can queue as a premade of two, and the game adds a third.
- **Why can't I play ranked with my friend in Apex?** More than two tiers apart, under Level 20, or the ranks on the platforms you're playing on are further apart than the ones you compared.
- **Can you play Apex ranked with PC and console friends?** Yes, with crossplay on, in PC lobbies (hedged per the section above).
- **Does playing with a higher-ranked friend cost more RP?** The premade is matched and charged at its highest player's RP, so yes: you pay their tier's entry cost, more in a Goliath match.

### Conclusion: Find your trio (~90 words)

Three or four bullets: within two tiers of each other; a premade plays and pays at its best player's rank; ranks are per platform; queue with people you trust. Then the call to post and the feed link. The official Apex Legends Discord, if EA runs one; check before writing, and name it only if it's EA's.

## Facts, with sources

| Fact | Source | Date |
|---|---|---|
| "You need to be at least Level 20 to play." | [How Apex Legends Ranked works](https://help.ea.com/en/articles/apex-legends/ranked/) | updated early Nov 2026 |
| "All players in a premade Ranked squad have to be within two ranks of each other. If they aren't, matchmaking will fail. This happens to prevent boosting and help keep the competition fair." | same | same |
| "Rank restrictions apply to all premade squads, including Duos and Trios. You'll be matched against other squads using the highest RP value in your premade squad." | same | same |
| "A Bronze player can play with Rookie, Bronze, Silver, and Gold players, but can't play with anyone ranked Platinum or higher." "Matchmaking rules can change during temporary tests." | same | same |
| Ranks Rookie to Master; "Apex Predators are the top 750 players per platform in the Master rank." Four divisions per rank. | same | same |
| Ranked progress per platform: "you could be Bronze rank on one platform but Gold on another"; PS4 and PS5 one platform | same | same |
| Entry cost: Rookie 0, Bronze 10, Silver 20, Gold 38, Platinum 48, Diamond 65, Master/Predator 90 RP; Challenger two tiers below the median pays less, Goliath two above pays more | same | same |
| Resets "twice per Season"; split two reset table (Diamond II and I to Platinum II 10,000 RP; Master and above to Platinum I 11,000 RP, and so on) | same | same |
| Season start placed by hidden Ranked skill; new players by Unranked Trios skill | same | same |
| Ranked Ladders: "You must have cross-play turned on", "with or without a squad" | same | same |
| "out of the 20 teams that entered the game" | same | same |
| Ranked and Unranked are Core modes; no Ranked Duos listed | [Every Apex Legends game mode](https://help.ea.com/en/articles/apex-legends/game-modes/) | updated about Apr 2026 |
| Crossplay on all platforms, on by default; "You can't turn cross-play off if you play on PC"; disabled, "you'll only be able to play with other players on the same platform as you who've also disabled cross-play" | [How to add friends for cross-play](https://help.ea.com/en/articles/apex-legends/add-friends-cross-play/) | updated about Aug 2026 |
| "Intentionally partying up with cheaters … counts as cheating"; repeated partying with confirmed cheaters may bring a ban | [Play by the rules](https://help.ea.com/en/articles/apex-legends/play-by-the-rules/) | updated about Aug 2026 |
| Squad Tier "calculated from the average RP of each squadmate"; "all premade squadmates' RPs are treated as equal to the value of the highest squadmate's RP within their premade group"; "A player who is not a part of your premade will have no RP adjustment"; Challenger −30%, Goliath +50% at 2+ tiers from the Lobby Tier; "Season 31 begins on November 3rd" | [Cipher Patch Notes](https://www.ea.com/games/apex-legends/news/cipher-patch-notes) | 2026-11-02 |
| Road to Ranked removed, "access to Ranked will move back to only requiring Account Level 20"; Season 32 "in early 2027": Iron, Emerald, Grandmaster, Open League and Pro-Am League (Diamond, Masters, Grandmaster) | [Ranked Roadmap](https://www.ea.com/games/apex-legends/news/ranked-roadmap-31-32) | 2026-10-20 |
| Test: "we limited squad-ups to 1 tier above or below your rank, instead of 2"; "Later on in Season 29, in all regions, Ranked will be solo-queue only for all players Diamond and above", a two-week window | [Matchmaking Tests Update](https://www.ea.com/games/apex-legends/news/overclocked-matchmaking-update) | 2026-06-08 |
| "we are not making the one-tier restriction permanent"; players were "used to playing with friends within two rank tiers from them"; "We are not rolling out the solo queue experiment as a permanent requirement for high-skill lobbies"; "That does not mean solo queue experiments are dead" | [Breaking Down Matchmaking Test Results](https://www.ea.com/games/apex-legends/news/matchmaking-test-results) | 2026-09-30 |
| Masters matched only with Masters and Diamond for about two weeks at the end of Season 26 Split 2 (a matchmaking test, not a party rule); "not split out premades and solo queuers" | [Matchmaking Test Update](https://www.ea.com/games/apex-legends/apex-legends/news/amped-matchmaking-update) | 2025-11-26 |

| "You will only be in PC crossplay lobbies if you have a PC player on your team," a Respawn engineer, on TikTok | [GamesRadar](https://www.gamesradar.com/you-will-only-be-in-pc-crossplay-lobbies-if-you-have-a-pc-player-on-your-team-says-apex-legends-dev) | 2020 |

## Unconfirmed, kept out or hedged

- **The table beyond Bronze.** EA gives one example. The other rows apply its stated rule; hedge with one sentence, and check any row a reader reports wrong.
- **Whether Predator counts as its own tier** for the party check. EA calls it a title within Master; the table treats it so. Hedged.
- **Which rank counts for a player who plays on two platforms.** Ranks are per platform; that the check uses the rank of the platform being played on is inferred. Hedged.
- **Console players in PC lobbies.** From a 2020 statement only; no current EA page says it for Ranked. Hedged as how Respawn described crossplay at launch, or left out.
- **Pages behind forums.ea.com's bot check,** among them the Season 26 matchmaking test post. Its ea.com counterparts were read; nothing in the brief rests on a forum-only page.
- **Split two's date in Season 31.** Not published. Not stated.
- **Season 32's effect on premades.** Not announced. The guide says so and names the recheck.
- **The 2020 claim of "three ranks".** Out of date; mentioned only as what older pages say, without a link, if at all.
- **Abandon penalties for a party.** Not researched; left out.

## Not checked in this session

This brief was prepared in a cloud session, without GUIDES.md, the production data or a browser that could pass EA's bot checks. To check on a computer before the guide is written:

**Evidence the workflow asks for**

- **GUIDES.md.** Its Search Console notes for Apex, and whether this topic or another Apex topic was already weighed. Neither the repo nor Drive holds it; Drive's copy of the planning folder is from 9 October and has no GUIDES.md.
- **The Google results page and AI Overview** for "apex rank difference party" and "apex ranked party restrictions". Only a search API's results were seen, not Google's page, so what the top results and the AI Overview say is unchecked. In particular, whether EA's older help copy with "three ranks" still ranks, and where.
- **Autocomplete** came from `suggestqueries.google.com` with `hl=en&gl=us`, not from the planning folder's suggest script, so it may differ from earlier briefs' method.
- **The pages that are wrong** weren't opened, only seen in search snippets: pley.gg's one-tier article (May 2026), the Dot Esports article giving three tiers (date unknown), and the old EA help URL `help.ea.com/en/help/apex-legends/apex-legends/apex-legends-ranked-mode`. Open each before the guide describes what other pages say, or say it without naming them.
- **TeamTavern's posts.** No backup was available. The counts under "Adding TeamTavern's data" are all still to do.

**Facts to confirm in the game or in a browser**

- **That premades work as the help page says today.** The two-tier rule comes from the help page ("Updated 3 weeks ago" on 2026-11-28) and the 30 September post. Neither test was checked in the client. Queue with a friend two tiers apart, and one three apart, if possible.
- **Every row of the tier table but Bronze's.** EA's example covers Bronze only. Check at least Rookie (does Rookie count as a tier for the rule?), Platinum with Master, and Predator (its own tier, or Master?).
- **Which rank counts for a player with ranks on two platforms.** Inferred to be the rank on the platform they're playing on.
- **No Ranked Duos.** Inferred from EA's list of modes, which has none. The help page's "including Duos and Trios" is read as premades of two and three, not as a Ranked Duos mode. Confirm in the mode selector.
- **Level 20.** The help page says it, and the roadmap says access "will move back" to Level 20. Confirm that Road to Ranked is gone in Season 31's client.
- **Entry costs.** The help page gives Platinum 48 and Diamond 65 RP. A fetch tool's summary said an image caption in the Cipher patch notes shows a 65 RP Platinum example. Look at that image: if it says Platinum 65, the help page or the caption is out of date.
- **Console players in a PC player's party.** The only source is a 2020 TikTok quoted by GamesRadar. Look for a current EA statement, or check in the game which lobby a mixed party gets.
- **Season 31's split two date.** Not published when this was written.
- **When Season 29's tests ran.** The 8 June post puts solo queue "later on in Season 29" for "a two-week window"; the exact dates weren't found.
- **That Ranked Arenas is gone.** Taken from the seed file's comment, not sourced this session. It matters only if the guide says why the Arenas one-tier rule no longer applies.
- **The help subpages' update dates** (game modes, cross-play, Cross Progression, play by the rules) are approximate, worked out from "Updated N months ago".
- **forums.ea.com.** Its bot check stopped both WebFetch and Chromium. Nothing in the brief rests on a forum-only page, but the Season 26 matchmaking test post was not read.
- **An official Apex Legends Discord.** Not checked. Name one in the closing section only if EA runs it.
- **Abandon and leaver penalties for a premade.** Not researched.

**Quotes**

Every quote from ea.com was checked against the full text fetched with curl. The help.ea.com quotes come from the page fetched with curl on 2026-11-28. None of them was read in a browser; a browser check of the help page's "Can't join friend's party?" section is worth doing, since that section is the guide's foundation.

## Adding TeamTavern's data

The production backup and the cluster's `team_tavern` copy aren't in this cloud session. Before writing, restore the latest backup the user provides and count:

- Apex player and group posts that tick "Ranked"
- group posts with a rank range wider than two tiers, which couldn't queue as one premade
- the platform split, if it says something about crossplay

Use the numbers only if they say something; otherwise leave the section's data line out.

## Internal links

- Opening paragraph: every game's team format, `/guides/join-an-esports-team#your-games-team-format`
- The closing section: the join guide's Apex section, for the Challenger Circuit (`/guides/join-an-esports-team#apex-legends`)
- **Join guide:** the Apex H3 gains the ranked sentence the siblings have: "Most trios start in Ranked, where a premade of two or three must be within two tiers and plays at its best player's rank. [Apex Legends squad size, party size and playing ranked with friends](/guides/apex-legends-ranked-with-friends) covers who can queue together and how to find a squad in range." Its `updated` moves.
- **Make guide:** no change. It doesn't link the ranked guides, and its Apex row is the Challenger Circuit's.

## Trust signals and schema

Organisation as author, Article JSON-LD as the siblings have, `games` naming `apex-legends` so the cover shows in the list and on shared links.

## Keeping it current

Recheck when:

- Season 32 arrives in early 2027, with its new ranks and leagues. This is the near-certain refresh: the table's tiers change, and possibly the premade rule.
- EA posts another matchmaking test, especially solo queue at the top ranks
- the help page's "Can't join friend's party?" section changes

## Implementation

As the skill sets out: the guide in `Server/Guide/apex-legends-ranked-with-friends.md`, registered in `Guides.js` and `Guides.purs`, the join guide's Apex H3, a test in `guides.spec.ts`, and the slug in `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and `screenshots.mjs`. The guide's one table is who each tier can queue with.
