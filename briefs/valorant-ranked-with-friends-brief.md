# Content Brief: Valorant team size, party size and playing ranked with friends

The eighth game guide, and the first for Valorant. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the Marvel Rivals, Rocket League, The Finals, Deadlock, Rainbow Six Siege and Overwatch briefs. It covers Competitive's party rules and what a player needs to know to play ranked with friends:

- how big a party can be
- which ranks can queue together as a duo or trio
- why a four can't queue
- why Immortal and Radiant players can't trio
- what a five-stack of any ranks costs
- what a friend new to the game has to do first
- how PC and console players group

Valorant is already on the site, so the guide goes out as soon as it's written, and the join guide and the make guide then link here.

Premier was the other candidate. It waits for a guide of its own once Riot publishes the next Stage (see Why this topic).

Prepared 2026-10-21. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, 25 Sep to 20 Oct 2026 (`now3-pq.tsv`), and 1 Jan 2025 to 20 Oct 2026 (`all3-q.tsv`)
- Google autocomplete for 41 Valorant seeds (`%LOCALAPPDATA%/Temp/gsc/suggest-valorant.mjs`, output `suggest-valorant.txt`)
- the Google results, with AI Overviews and People Also Ask, for "valorant ranked with friends", "valorant what ranks can play together" and "valorant party rank restrictions", plus seven Premier searches, loaded on 2026-10-21 with `hl=en&gl=us`
- Riot's Competitive Mode FAQ (dated 17 May 2026), read from its HTML; the patch notes it points to (3.10, November 2021, and 5.0, June 2022); every 2026 patch note from 12.00 to 13.08, read for party changes; Riot's July 2026 fair-play post; and the Console Guide
- r/VALORANT and r/valorantindia threads from October 2025 to October 2026, read only
- the 2026-10-21 production backup, restored into a throwaway container (`%LOCALAPPDATA%/Temp/gsc/valorant-posts.sql`, output `valorant-posts.txt`; group ranges in `valorant-ranges.csv`, checked against Riot's table by `valorant-ranges.mjs`)

## Why this topic

The user chose it on 2026-10-21 over Premier, after the handover's recheck (`briefs/valorant-handover.md`):

- **The pattern that works.** Guides that answer how a game's mode treats friends rank at 4–8 within days.
- **Autocomplete is full, fuller than the handover recorded.** These seeds each give ten suggestions: "valorant rank difference", "rank disparity", "5 stack", "4 stack", "can ascendant play with", "can immortal play with" and "crossplay ranked". "Duo rank" and "party size" give eight. "Can silver/gold/plat/diamond play with …" each return Valorant forms, among them "can silver play with plat valorant", "can gold play with diamond valorant" and "can diamond play with gold valorant". "Valorant ranked with friends" gives seven, among them "why cant i play ranked valorant with friends" and "valorant rank difference to play with friends". "Can you play ranked with" gives nine, led by "4 people".
- **The rule is stable and on one Riot page.** The Competitive Mode FAQ was last dated 17 May 2026. No patch note from 2025 or 2026 changes a party rule. The guide needs checking only when Riot announces a change, the next possible one being 14.00 (see Keeping it current).
- **Valorant is the site's busiest feed** (13,435 player posts since 2021). Ranked is what its posts are for: 85% of player posts and 79% of group posts tick it.
- **No Valorant ranked query reaches the site.** The feed's queries are "team finder", "lfg", "party finder" and "5 stack finder", so the case rests on autocomplete, as Marvel Rivals' and Overwatch's did.

**Why not Premier now.** Premier's biggest queries are "schedule" and "times". Riot's own answer to "times" holds across Stages: [Queues & Matchmaking](https://support.riotgames.com/en-us/valorant/gameplay/premier-queues-matchmaking) says a zone's matches open around 7 PM local time, between 7 and 9 PM. But each Stage's length and playoff bar change every Act. In 2026 the Stages ran five, six, six, seven and five weeks, with bars between 450 and 600.

Act 6 began on 13 October with patch 13.08, which has no Premier section. Riot hadn't published the new Stage's dates or playoff bar when this brief was written; only Strafe gives 21 October to 6 December. Strafe (17 October) and PaxJax (5 October) are both fresh, and the Premier option on TeamTavern's posts has been ticked six times, so a Premier guide has no data of its own yet.

A Premier guide launched at the start of a Stage, from that Stage's patch notes, would take over the join guide's Premier facts, as the Marvel Rivals Championship guide did. The next-guide check should consider it once Riot posts the Stage.

## Template

**Recommended**: `how-to-guide`. The reader wants to know whether they can queue with a particular friend, why the game won't let them, and what to do instead: a duo, a five-stack, or another mode. What sets the guide apart:

- Riot's table turned into worked examples at its edges
- the fact that below Platinum it counts whole ranks, not divisions
- what "one tier" means in Riot's own example
- the five-stack's real costs, from Riot
- what a friend new to the game, or on console, has to do

## Target keywords

Search Console has no Valorant ranked volume of its own. This guide targets what a listing can't answer:

- **Primary:** valorant ranked with friends, valorant what ranks can play together, valorant rank difference, valorant party rank restrictions
- **Secondary:**
  - valorant rank disparity, rank disparity chart, rank disparity rules, rank gap to play with friends, max rank difference
  - valorant duo rank restrictions, duo rank difference, duo rank limit
  - valorant 5 stack rules, 5 stack rank disparity, 5 stack rr reduction, 5 stack queue time, can you 5 stack
  - valorant 4 stack ranked, can you play ranked with 4 people, why can't you play ranked with 4 people
  - valorant party size, ranked party size, invalid party size, team size, competitive team size
  - can silver play with gold / plat, can gold play with diamond, can diamond play with gold, can ascendant play with diamond / immortal, can immortal play with ascendant / radiant, can unranked play with plat
  - valorant crossplay ranked, can ps5 and xbox play ranked together
  - valorant ranked requirements, level 20
- **Leave to the feed:** "valorant lfg", "team finder", "party finder", "5 stack finder", "duo finder", "looking for group"
- **Leave to the join guide, and to a later Premier guide:** "valorant premier", "premier team finder", "valorant teams to join", "how to join a valorant esports team"
- **Leave out:**
  - rank distribution, RR per win, the rank list in full, Act Rank and rewards. Stats and booster sites own them.
  - climbing tips and agent advice. That's gameplay.
  - hidden MMR beyond one sentence. Riot doesn't publish it.
- **Questions:**
  - What ranks can play together in Valorant? (People also search for: "can silver 1 play with gold 3", "can silver play with plat", "can bronze silver and gold play together")
  - Can you play ranked with 4 people in Valorant? (autocomplete)
  - Can you 5-stack in Valorant ranked? (People also search for)
  - Can unranked play with Plat? (People also search for)
  - Why can't I play ranked with my friends? (autocomplete)
  - Can PS5 and Xbox play ranked together? (People also search for: "ps5")
  - How big is a Valorant team? (autocomplete: "team size", "competitive team size")

Measure after launch instead of guessing volumes: see Distribution.

## Search intent

The intent is informational. The reader has a friend the game won't queue them with, or a group that wants to play Competitive together and needs to know who fits, or whether to go five.

What ranks, loaded on 2026-10-21 (hl=en, gl=us):

- **"valorant ranked with friends":** Riot's Competitive Mode FAQ first, then GameTree (12 Aug 2026), Reddit, Eloboss (the URL now 404s), YouTube, GameBoost (modified 20 Oct 2026), Eloboost24 (March 2026) and the Fandom wiki's Competitive page. Google removed two results on a legal request. **The AI Overview is wrong** on two points:
  - It says Silver and Gold must be "within 1 tier". Riot lets a Silver go up to any Gold.
  - It says trios aren't allowed from Ascendant 3. Riot's line is Immortal 1.
  
  People also search for: "ps5", "can bronze play with gold", "can unranked play with plat", "can you 5 stack".
- **"valorant what ranks can play together":** Riot's FAQ, GameTree, Eloboss, a Reddit thread (March 2026), Gamingcy (16 Sep 2026), Eloboost24, thespike.gg (2023). The AI Overview is close to Riot's table. It cites an official Valorant TikTok on rank disparity (September 2025).
- **"valorant party rank restrictions":** Riot's FAQ, Fandom, GameTree, Eloboss, Reddit, Gamingcy, GameBoost, and a Facebook answers page that says five-stacks are capped at Ascendant, which is wrong. The AI Overview is essentially Riot's FAQ, and accurate.

**The competitors, and what each gets wrong:**

- **Riot's Competitive Mode FAQ** ranks first and is right, but it's a table and a few lines.
  - Its wording confuses players. It says each rank "has three tiers", then gives Platinum and up "Exactly 1 tier higher max", with the example "Platinum 2 can queue with up to Diamond 2", which is one rank, not one tier.
  - It works no examples at the edges.
  - It says nothing about new friends below level 20 or console.
  - It says nothing about what a five-stack costs beyond the RR cut.
- **GameTree, "What Ranks Can Play Together"** (12 Aug 2026, title still "2025"). Its meta description says "within 3 rank tiers", against its own body. It says "unranked can play with ranked players up to Gold" with no source, and carries a boosting plug.
- **GameBoost** (modified 20 Oct 2026, about 450 words): Riot's table and the five-stack penalties. It leaves out the Immortal duo rule, the four-stack ban and new friends. It says the penalty cuts "gains, not losses", which Riot doesn't say.
- **Gamingcy, Ranks Guide** (16 Sep 2026). Mostly right, including Immortal 1+ with no trios and PC and console ranked separately, but party rules are one section of a ranks page.
- **Eloboost24, Eloboss, thespike.gg.** Boosting or old pages.
- **No page** gives:
  - worked examples at the edges (Silver 1 with Gold 3 yes; Gold 3 with Diamond 1 no)
  - the plain reading of "one tier"
  - why below Platinum it's ranks, not divisions
  - Riot's 2026 word that five-stacking with friends of any rank is fine, and what a five-stack costs in queue and RR
  - level 20 for a new friend, and the alternate-account line Riot drew in July 2026
  - console with sources

So a page that gives Riot's rule worked out for every rank, and the five-stack, new-friend and console answers, has something to add to every one of these searches. It also corrects the AI Overview that ranks for "valorant ranked with friends".

Say in the first paragraph who the guide is for: a player who wants to play Competitive with friends of other ranks, or whose friend the game won't queue with.

**Players hit the rules without understanding them.** From Reddit (read only; the guide doesn't link Reddit):

- "you can't queue with someone 2 or more ranks different than you (ex. Silver and Plat)" (2026-04-03), a rule of thumb that fails at Gold 3 and Diamond 1: "A gold 3 and diamond 1 cannot q together" (2026-03-23)
- Bronze 3 and Gold 1 refused (2026-04-03); Bronze and Gold (2026-08-04)
- "It completely blocks 4 friends from playing comp together" (2026-03-19)
- "If I can't duo/trio with my friends due to rank disparity, I should not be matched with players in those ranks either" (2026-04-24, 325 points)
- an Ascendant 3 five-stacking with Gold and Platinum friends: "+12-14rr for wins and -18-22rr for losses" (2026-10-12)
- "How do I get to level 20 fast? I'm trying to play ranked with my friend" (2026-09-25)
- "No you still have a rank just hidden", to a player who hoped going unranked would help (2026-10-18)

The guide answers each:

- the first two want the table and the edge cases
- the third wants the four-stack rule and its reason
- the fourth wants a sentence on visible rank and matchmaking
- the fifth wants the five-stack's costs
- the sixth wants level 20
- the last wants the line that a rank counts whether shown or not

## Content parameters

- **Word count:** 1,600–2,000. The join guide's Valorant H3 gains one sentence and links here.
- **Reading level:** plain and direct, like the other guides. It says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the other guides, Markdown in `Server/Guide/`.
- **H2 sections:** 5, plus an FAQ.
- **Visuals:**
  - the game's cover, as in the other guides
  - one table: who can queue with whom, by the lowest rank in the party (see the outline). The specs' `getByRole("table")` is strict, so there's only one table. Three columns, five rows, checked at 375 px against `phone.spec.ts`.
  - the five-stack's RR cuts as a short list, not a second table
  - no chart (see Adding TeamTavern's data)
- **FAQ:** 6 questions.
- **Naming:**
  - "Valorant", as the seed writes it. Riot writes VALORANT; the guide doesn't shout.
  - "Competitive", as Riot names the mode, and "ranked" where players' words are wanted. Say once that they're the same.
  - "Party", as Riot's FAQ says, and "duo", "trio" and "five-stack" where players' words fit. Riot writes "5-stack"; the guide writes "five-stack" in prose.
  - Ranks as the game and the seed write them: "Gold 3", "Diamond 1", "Radiant".
    - A "rank" is Iron, Bronze and the rest; 1 to 3 are its divisions, 3 the highest.
    - Riot calls them "tiers", and its table's "one tier higher" means one rank up, at the same number. Say that once, plainly, with Riot's own example. The guide says "rank" and "division" throughout.
  - "RR" (Rank Rating), explained once.
  - "Platform" as the seed has it: PC, PlayStation, Xbox.
  - "Unrated" and "Swiftplay" for the unranked modes, as the client names them.

## Recommended title

**Valorant ranked with friends: what ranks can play together, five-stacks and party size**

Alternatives:

1. Valorant rank difference: who you can duo, trio and five-stack with
2. Can you play ranked with friends in Valorant? Party size and rank limits

H1: "Valorant team size, party size and playing ranked with friends", as the Deadlock, Marvel Rivals and Overwatch guides' H1s carry "team size", which the title has no room for.

Slug: `valorant-ranked-with-friends`. It's permanent, so it names the subject, not an Act.

## Meta description

Valorant is 5v5. A duo or trio queues ranked within Riot's rank limits, four can't queue, and a five-stack can be any ranks, at a cost in RR and queue time.

For `Guides.purs`, which the other guides keep shorter: "Valorant team size is five. Ranked with friends: what ranks can duo or trio, why four can't queue, and what a five-stack of any ranks costs."

## TL;DR draft

> - Valorant is played five against five. In Competitive you can queue alone, as a duo, as a trio or as a five-stack, but not as four.
> - A duo or trio goes by its lowest-ranked player. If the lowest is Iron or Bronze, the highest can be up to Silver 3; Silver goes up to Gold 3, and Gold up to Platinum 3. From Platinum up, the highest can be one rank above at the same number or below it, so Platinum 2 goes up to Diamond 2.
> - Immortal and Radiant players can only play solo, as a duo or as a five-stack.
> - A five-stack can be any ranks. It plays only other five-stacks, so it waits longer, and its RR changes are cut by 25% when its ranks are wider than a duo's could be, more with a Radiant.
> - Competitive opens at account level 20, so a friend new to the game plays Unrated or Swiftplay with you until then.

## Information gain

- **[UNIQUE INSIGHT] Below Platinum it's whole ranks, not divisions.**
  - The table gives a Silver anyone up to Gold 3, and a Gold anyone up to Platinum 3, so Silver 1 and Gold 3 can duo, five divisions apart. Gold 3 and Diamond 1, four divisions apart, can't, because Diamond is two ranks above Gold.
  - From Platinum up, it's three divisions at most: Platinum 1 reaches only Diamond 1.
  - Iron and Bronze share a row, so Iron 1 can duo with Silver 3, eight divisions up.
  - The window narrows as players climb, and players' "two ranks apart" rule of thumb is wrong both ways. No page says this.
- **[UNIQUE INSIGHT] What "one tier higher" means.** Riot's FAQ says each rank has three tiers, then allows Platinum and up "Exactly 1 tier higher max", with "Platinum 2 can queue with up to Diamond 2" as the example. That example is one rank up at the same number, three divisions. [Patch 5.0](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-5-0/) said the same for Ascendant: "3 ranks above or below". Say it once, with Riot's example, and the confusion behind GameTree's "within 3 rank tiers" goes away.
- **[UNIQUE INSIGHT] Five-stacks, in Riot's 2026 words.** Riot's [July 2026 fair-play post](https://playvalorant.com/en-us/news/announcements/playing-fair-anti-boosting-and-other-smurfing-countermeasures/) lists "Five-stacking with friends, even if you are all different ranks" as allowed: "this is not about stopping people from playing with their friends." With the FAQ's RR cuts and "only be matched with other parties of 5", that answers the whole five-stack question in one H2.
- **[UNIQUE INSIGHT] A new friend, and the alternate-account line.** Competitive needs account level 20. The same post allows "Playing on an alternate account that you created and leveled yourself". It also says a player who queues with someone engaged in rank manipulation can lose the rating gained. So a lower alt of your own is allowed, but playing on someone else's account, or carrying a friend on one, isn't.
- **[CORRECTION]** Two points from the AI Overview for "valorant ranked with friends", stated plainly without naming Google:
  - Silver and Gold aren't held to one tier.
  - The trio limit starts at Immortal 1, not Ascendant 3.
- **[DATA] TeamTavern's posts.** See Adding TeamTavern's data.

## Content outline

### Introduction (~110 words)

- **Hook:** Valorant lets any five friends queue Competitive together, but a duo or trio has to fit Riot's rank limits, four can't queue at all, and a five-stack of mixed ranks pays in RR. The rule people quote ("two ranks apart") is wrong both ways.
- **Who it's for:** a player who wants to play Competitive with friends of other ranks, or whose friend the game won't queue with.
- **Promise:** party sizes, which ranks can duo or trio, Immortal and Radiant, what a five-stack costs, friends new to the game, PC and console, and finding players in range. Link [every game's team format](/guides/join-an-esports-team#your-games-team-format) for Premier and other games.
- The TL;DR box goes here, as "In short", like the other game guides.

### H2: Team size and party size

- **Answer first:** Valorant is played five against five, in Competitive as in Unrated. In Competitive a party can be one, two, three or five players, never four.
- **Four can't queue**, cited to Riot's [Competitive Mode FAQ](https://support.riotgames.com/en-us/valorant/gameplay/valorant-competitive-mode-faq): "Four-player groups are not allowed, as it tends to leave the remaining solo queue player on the team with a less-then-desirable experience." Unrated and Swiftplay take a party of four.
- **Unlocking Competitive.** Competitive opens at account level 20, by the same FAQ, for every player in the party.
- **Premier is separate.** A Premier team has five to seven players of any rank, and the team, not the party, is placed by its top five players' MMR. One or two sentences, linking the join guide's Valorant section (`/guides/join-an-esports-team#valorant-premier`).
- Game cover beside the first paragraph, as in the other guides.

### H2: Which ranks can queue together

- **Answer first:** a duo or trio goes by its lowest-ranked player.
  - If the lowest is Iron or Bronze, the highest can be up to Silver 3.
  - If the lowest is Silver, the highest can be up to Gold 3.
  - If the lowest is Gold, the highest can be up to Platinum 3.
  - From Platinum up, the highest can be at most one rank above the lowest, at the same number: Platinum 2 with up to Diamond 2.
  
  Cite the [Competitive Mode FAQ](https://support.riotgames.com/en-us/valorant/gameplay/valorant-competitive-mode-faq) on the words that name it.
- **Table: who can queue with whom.** Columns: Lowest rank in the party / Highest rank allowed / For example. In the game's order:

  | Lowest rank in the party | Highest rank allowed | For example |
  |---|---|---|
  | Iron or Bronze | Silver 3 | Iron 1 with Silver 3: yes. Bronze 3 with Gold 1: no |
  | Silver | Gold 3 | Silver 1 with Gold 3: yes. Silver 3 with Platinum 1: no |
  | Gold | Platinum 3 | Gold 1 with Platinum 3: yes. Gold 3 with Diamond 1: no |
  | Platinum, Diamond or Ascendant | One rank up, same number | Diamond 1 with Ascendant 1: yes. Diamond 1 with Ascendant 2: no |
  | Immortal | One rank up, same number, as a duo only | Ascendant 2 with Immortal 2: yes, but no third player |

  How it's derived, for the writer: number the divisions Iron 1 = 1 to Immortal 3 = 24, Radiant = 25 (as the seed does). Below Platinum the highest allowed is the last division of the next rank: Iron and Bronze up to 9, Silver to 12, Gold to 15. From Platinum up it's the lowest + 3. Players' reports agree with every example: Gold 3 with Diamond 1 refused, Bronze 3 with Gold 1 refused, Silver and Platinum refused.
- **"One tier" means one rank up.** Riot's FAQ says each rank "has three tiers" and then allows Platinum and up "Exactly 1 tier higher max", with the example "Platinum 2 can queue with up to Diamond 2". The example is the rule: one rank up at the same number, three divisions. Patch [5.0](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-5-0/) put it as "3 ranks above or below" for Ascendant. Two sentences.
- **It's ranks, not divisions, below Platinum.** Silver 1 and Gold 3 can duo, five divisions apart, while Gold 3 and Diamond 1, four apart, can't. The limit tightens as players climb: Iron 1 reaches Silver 3, but Platinum 1 only Diamond 1. So "two ranks apart" is wrong both ways: Iron with Silver is fine, and Gold with Diamond never is.
- **A trio is checked on its lowest and highest.** The middle player only has to sit between them. Add a third player and the party may stop fitting if they're lower than both.
- **Visible rank, not MMR.** Players report that the party check uses the rank they see, while matchmaking uses hidden MMR, so a party can meet players it couldn't have queued with. One sentence, stated as what happens, uncited. Riot's [13.00 notes](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-13-00) say matchmaking keeps each team's average rank within one division of the other's; cite only if it fits.
- **A hidden rank still counts.** A player whose rank isn't shown still has one, and it's what the check uses. One sentence, stated plainly.

### H2: Immortal and Radiant

- **Answer first:** from Immortal 1 up, a player can queue only solo, as a duo, or as a five-stack, never as a trio. Cite the FAQ: "Players in Immortal 1 and above can only solo, duo, or 5-stack."
- A duo with an Immortal follows the Platinum-and-up rule: an Ascendant 1 can duo with up to Immortal 1.
- **Radiant** has no divisions, and Riot doesn't spell out its partner. Players report a Radiant duoing with an Immortal 1 in April 2026. State the effect as what the game allows, hedged, with no rule.
- **The line moved before.** Patch 5.0 moved the solo/duo/five-stack limit from Diamond 3 up to Immortal 1 when Ascendant was added. So older pages, and an AI answer that says "Ascendant 3", are out of date. One sentence, no names.
- Short H2: about 150 words.

### H2: Five-stacks: any ranks, at a cost

- **Answer first:** a full party of five can be any ranks: "you can play with 4 of your friends no matter what your skill levels are" ([FAQ](https://support.riotgames.com/en-us/valorant/gameplay/valorant-competitive-mode-faq)). Riot's [July 2026 fair-play post](https://playvalorant.com/en-us/news/announcements/playing-fair-anti-boosting-and-other-smurfing-countermeasures/) lists "Five-stacking with friends, even if you are all different ranks" among what's allowed.
- **What it costs**, from the FAQ, as a short list:
  - Every five-stack plays only other five-stacks ("Groups of 5… will only be matched with other parties of 5"), so it waits longer.
  - Everyone Ascendant 3 or below: a 25% RR cut if anyone is outside the duo-and-trio limits above. A five-stack of Silvers and Golds plays at no cut.
  - Anyone Immortal 1 to 3: a 25% cut, whatever the others' ranks.
  - Anyone Radiant: a 75% cut, and 90% if anyone is below Radiant.
- **RR explained once:** the points a player gains for a win and loses for a loss.
- **Gains, losses, or both.** Riot's FAQ says "RR reduction"; the 3.10 notes said "Rank Rating gains and losses will be reduced". One player's report (+12–14 per win, −18–22 per loss) suggests losses weren't cut for them. The guide says the RR the team wins or loses changes less, cites the FAQ, and doesn't claim gains only.
- **What to do:** a five-stack inside the duo-and-trio limits plays at full RR. A wider group that wants full RR splits into duos and trios that fit. A group that plays for fun accepts the cut. A four, in Competitive, needs a fifth or has to split.

### H2: New friends, PC and console, and finding players in range

- **A friend new to Valorant. Answer first:** Competitive opens at account level 20, so a new friend plays Unrated or Swiftplay with you until then.
  - **Alternate accounts:** by Riot's July 2026 post, playing "on an alternate account that you created and leveled yourself" is allowed. Queuing with a player engaged in rank manipulation can cost the rating gained ("Hitchhikers can also have their rank reverted…"), and playing on an account that isn't yours isn't allowed.
  - **Friends in placements:** Riot doesn't say how a friend in placements, or without a rank yet, counts for a party (see Not confirmed). The guide says to check in the client and leaves the rule out.
- **PC and console. Answer first:** console players play ranked against console players only, PlayStation and Xbox together, with "a separate rank and MMR" from PC ([Console Guide](https://support.riotgames.com/en-us/valorant/gameplay/valorant-console-guide)). So a PC player and a console player can't queue Competitive together, while a PlayStation and an Xbox player can. Keyboard and mouse on console aren't supported. Whether console Competitive uses the same party limits isn't stated; the guide says the FAQ's table is Riot's one rule.
- **Finding players. Answer first:** recruit players whose rank fits everyone already in the party, by the table, in the roles you're short of, on the same platform, at the hours you play.
  - **Range first.** Ranks move every match, so leave room: a duo at the edge of the table won't fit after one rank-up.
  - **Three or five, not four.** A four can't queue, so recruit to three or five.
  - **Roles.** A full five usually wants a Duelist, an Initiator, a Controller and a Sentinel, plus a flex. Name the roles you're short of.
  - **Keep a good teammate.** After a good match, add them by Riot ID. One sentence.
  - **From ranked to a team.** A five that plays every week and wants more can make a Premier team; link the join guide's Valorant section rather than repeating it.
  - One line linking the make guide for keeping a team together: "How to make an esports team".
- **The TeamTavern step:**
  - A player puts up a player post with their rank, roles and platform, and "Ranked" ticked under Looking for.
  - A group looking for players puts up a group post with a rank range a duo or trio can actually queue with, by the table. For a five-stack, the range is one the stack can play at full RR, not "Iron to Radiant".
  - Then filter the posts by rank, role and platform and message the ones that fit.
  - Link `/games/valorant` with the anchor "Valorant players and groups on TeamTavern".
  - TeamTavern's group-range figure goes here (see Adding TeamTavern's data).

### FAQ (6)

1. **How many players are on a Valorant team?** Five. Competitive takes parties of one, two, three or five.
2. **Can you play ranked with 4 people in Valorant?** No. Riot doesn't allow four-player parties in Competitive, because the fifth player would be a solo with a premade four. Unrated and Swiftplay take four.
3. **What ranks can play together in Valorant?** A duo or trio goes by its lowest player. Iron or Bronze with up to Silver; Silver with up to Gold; Gold with up to Platinum; from Platinum up, one rank higher at the same number at most. A five-stack can be any ranks.
4. **Can Gold play with Diamond in Valorant?** Not as a duo or trio. Gold reaches only Platinum 3. A Gold and a Diamond can play together as part of a five-stack, with a 25% RR cut.
5. **Can you 5-stack with any ranks?** Yes. A five-stack plays only other five-stacks and takes an RR cut when its ranks are wider than a duo's could be, or when it has an Immortal or Radiant.
6. **Can PS5 and Xbox players play ranked together?** Yes: console Competitive crosses PlayStation and Xbox. PC and console don't play together, and each has its own rank.

### Conclusion (~90 words)

- Takeaways:
  - the lowest player sets the limit
  - below Platinum count ranks, from Platinum count three divisions
  - never four
  - Immortal and up duo at most
  - a five-stack can be anyone, at a cost
  - level 20 first
- **Call to action:** "Valorant players and groups on TeamTavern".

## Facts, with sources

Checked 2026-10-21.

**Riot's own pages. Cite these:**

- **[Competitive Mode FAQ](https://support.riotgames.com/en-us/valorant/gameplay/valorant-competitive-mode-faq)**, dated 17 May 2026, read from its HTML:
  - "you'll only be able to join a Competitive match with friends within a specific rank range."
  - Party Restrictions: "For parties of 2 or 3, players can queue within certain ranked disparity restrictions". Its table:

    | Lowest Rank in Party | Highest Rank in Party |
    |---|---|
    | Iron and Bronze | Silver |
    | Silver | Gold |
    | Gold | Platinum |
    | Platinum, Diamond, Ascendant, Immortal, and Radiant | Exactly 1 tier higher max (Example: Platinum 2 can queue with up to Diamond 2) |
  - "With the exception of Radiant, each rank has three tiers. The higher the number, the better your rank."
  - "Parties of 5 (5-stacks) can ignore the above chart, as they are not bound by any ranked restrictions. However, 5-stacks of Iron-Ascendant players will receive RR penalties if any players are ranked outside of a normal party restriction. (For example, a 5-stack of all Silver and Gold players will not receive RR penalties. A 5-stack of Silver, Gold, and Ascendant players will receive an RR penalty.)"
  - "Groups of 3 players or less must meet the skill disparity requirements listed above. Four-player groups are not allowed, as it tends to leave the remaining solo queue player on the team with a less-then-desirable experience."
  - "Players in Immortal 1 and above can only solo, duo, or 5-stack."
  - "full parties of 5 are allowed with no ranked restrictions. That means you can play with 4 of your friends no matter what your skill levels are. You could have a team of 3 Irons and 2 Ascendants…"
  - "If everyone in your 5-stack group is Ascendant 3 or below, the team will receive a 25% RR reduction if any players are ranked outside of normal party restriction ranges."
  - "If one or more members of your 5-stack group are Immortal 1 to Immortal 3, the team will receive a 25% RR reduction penalty."
  - "If one or more of your 5-stack group are Radiant, the team will receive a 75% RR reduction. Having any players in your group that are below Radiant automatically reduces your potential RR by 90%."
  - Five-stacks "will only be matched with other parties of 5", and so can expect longer queues.
  - "you'll need to be at least Account Level 20 before gaining access to Competitive Mode." Players who played ranked before Episode 4 are exempt; leave that out.
  - Nothing on placements or unranked players in parties.
- **[Patch 3.10](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-3-10/)** (16 Nov 2021): "we're removing all ranked restrictions from 5-stack parties"; "we are also removing 4-stacks from the Competitive queue"; "Rank Rating gains and losses will be reduced when playing in a 5 stack outside of our current rank restrictions rules." Its percentages are superseded; cite it only for "gains and losses", if at all.
- **[Patch 5.0](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-5-0/)** (16 Jun 2022): Ascendant added between Diamond and Immortal; "Grouping restrictions for Ascendant are 3 ranks above or below the Ascendant ranked player"; "This fits in with our Platinum+ rule set for grouping."; "Solo/Duo/5-stack restriction now starts at Immortal 1 (previously Diamond 3)"; "Five stacking 25% RR penalty now starts at Immortal 1 (previously Diamond 3)".
- **[Playing fair: anti-boosting and other smurfing countermeasures](https://playvalorant.com/en-us/news/announcements/playing-fair-anti-boosting-and-other-smurfing-countermeasures/)** (14 Jul 2026): allowed: "Five-stacking with friends, even if you are all different ranks"; "Playing on an alternate account that you created and leveled yourself". "To be clear: this is not about stopping people from playing with their friends." "If you are playing the game normally and competing to win, queueing with friends remains completely fine." Not allowed: "Queuing with another player engaged in any of the above". "Hitchhikers can also have their rank reverted for any rating gained by queuing with a player engaged in rank manipulation". "Play only on your own accounts."
- **[Console Guide](https://support.riotgames.com/en-us/valorant/gameplay/valorant-console-guide)** (2 Jun 2026 per the research; the page's metadata says 2024): "VALORANT players on Console cannot face off against VALORANT players on PC—you'll have a separate rank and MMR when playing on either platform."; "VALORANT supports crossplay across consoles"; "Keyboard and mouse are not supported, and attempts to use them on console (like spoofing) will result in a ban."
- **No party change in 2025–2026.** Patch notes 12.00 (6 Jan), 12.04, 12.08, 13.00 (23 Jun), 13.04 (18 Aug), 13.05, 13.06 (22 Sep) and 13.08 (13 Oct, Act 6) change no party rule. 13.00: "the average rank of each team is within 1 sub-tier". 12.01: Ascendant and above need Riot Mobile multi-factor authentication for Competitive; not for the guide. **13.06 announces "Competitive Flex (14.00)" and "updates for the Radiant experience (14.00)"**, which could touch parties.

**From the client, through players' reports.** Enough to state as what the game shows, never cited:

- Refusals that match the table:
  - Gold 3 with Diamond 1 (2026-03-23, 2026-08-04)
  - Bronze 3 with Gold 1 (2026-04-03)
  - Silver with Platinum (2026-04-03)
  - Bronze with Gold (2026-08-04)
- The party check uses visible rank, matchmaking uses MMR (many threads, 2026)
- A Radiant duoed with an Immortal 1 (2026-04-24, from a screenshot)
- Five-stacks queue at any ranks, and players report the 25% cut (2026-03-03) and harder lobbies (2025-12-14)
- A hidden rank still counts (2026-10-18)
- Nobody quotes the client's refusal text. The guide quotes none.

**Not confirmed, or sources disagree.** The guide handles each like this:

- **Friends in placements, or with no rank.** Riot's FAQ is silent. The fan wiki says a player in placements parties on their last Act's rank, and GameTree's "unranked can play up to Gold" has no source. The guide leaves the rule out and tells the reader to check in the client.
- **How many placement matches.** Third-party pages give five for a new account, and one or three between Acts. Not needed; leave out.
- **Radiant's partner.** "Exactly 1 tier higher" from Immortal: whether Radiant counts as the rank above Immortal 1, 2 or 3 isn't stated. One report has a Radiant with an Immortal 1. The guide hedges, as above.
- **Does the RR cut apply to losses?** Riot's 2021 notes say gains and losses; the FAQ says "reduction"; one 2026 report suggests losses weren't cut. The guide says the team's RR changes are cut, without "gains only".
- **Console party limits.** The Console Guide doesn't repeat the table. The guide says the FAQ's table is Riot's rule and doesn't claim console differs.
- **Patch 14.00.** "Competitive Flex" and Radiant updates are announced without detail. Check them when the notes are out.

## Adding TeamTavern's data

**Counted while writing**, from the 2026-10-21 production backup (`valorant-posts.sql`, output `valorant-posts.txt`):

- 13,435 player, 1,275 group and 198 community posts, from January 2021 to October 2026. 15 player and 6 group posts are from after the relaunch, so the figures are about who has posted since 2021, and the guide says so.
- Ranked is ticked on 85% of player posts (11,355) and 79% of group posts (1,011). Scrims and tournaments: 29% and 48%. Premier: 3 and 3, since the option arrived with the relaunch.
- Platform: PC on all but one post. Console arrived after almost every post was made, so leave platform out.
- **Group ranges:** 975 Ranked group posts give a closed rank range. **310 of them (32%) ask for a range whose lowest and highest ends couldn't queue together as a duo or trio** by Riot's table, so the group can only queue with both as a five-stack. 126 of those reach Immortal or Radiant from below, where a trio isn't allowed at all.
- **Caveat for the writer:** ranks carried over from the old site are whole ranks. Player ranks are stored as division 2 (Iron 2, Bronze 2, …); group ranges as a whole rank, "Bronze 1" to "Silver 3". Ascendant barely appears (3 player posts), because most posts predate it. The group-range figure is counted at the ranges' stated ends and holds at the rank level.

Use the group-range figure in the finding H2, with "TeamTavern's posts, 2021–2026, counted 21 October 2026", as the other guides do. No chart: one figure doesn't need one, and the join guide already charts roles.

## Internal links

- **Links from this guide:**
  1. `/games/valorant`, twice: in the finding H2 and in the conclusion. Anchor: "Valorant players and groups on TeamTavern".
  2. The join guide, from the introduction ("every game's team format") and from Team size and the finding H2 (its Valorant section, `/guides/join-an-esports-team#valorant-premier`, for Premier).
  3. The make guide, from the finding H2. Anchor: "How to make an esports team".
- **Links to this guide:**
  1. The join guide, in its "Valorant: Premier" H3, one sentence before the feed link: most players start in Competitive, where a duo or trio queues within Riot's rank limits and a five-stack can be any ranks. Link here with the anchor "Valorant team size, party size and playing ranked with friends", as its other H3s link their guides by H1.
  2. The make guide, a row in its format table, before the Premier row: **Valorant: Competitive** | Nobody: a party of 1, 2, 3 or 5 queues together | A duo or trio within Riot's rank limits; no parties of 4; Immortal and up duo at most; a five-stack of any ranks loses some RR.
  3. The `/guides` index, and the guide's `games` list naming `valorant`, so it shows on the guides list with its game.
- Editing the join and make guides adds a fact, so their `updated` changes too.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, and logo-512.png as `image`, as the other guides have. BreadcrumbList Home > Guides > the H1. `datePublished` and `dateModified`. No Person.
- Every rule the guide cites links to Riot's own page, on the words that name it, once per source per section. What comes from players' reports (the visible-rank check, the Radiant duo) is stated as what the game does, without a link. Reddit and community Discord servers are neither linked nor named.
- No "when we played" stories. Nobody here plays the game.

## Keeping it current

The party rules last changed in June 2022 (patch 5.0). A Valorant Act runs about two months.

- **Patch 14.00**, announced in 13.06 with "Competitive Flex" and Radiant updates, probably at the turn of the Episode in early 2027. Read its notes for "party", "group", "5-stack", "Immortal" and "Radiant".
- **The Competitive Mode FAQ.** Check its date at each Act's start.
- **Each Act's patch notes**, with the same words.

Change `updated` only when a fact changes.

## Implementation

- `Server/Guide/valorant-ranked-with-friends.md`, imported as text in `Guides.js`, and an entry in `Guides.purs`: slug, heading, title, description, published, updated, and `games: [ { handle: "valorant", title: "Valorant" } ]`.
- The sentence in the join guide's Valorant H3 and the row in the make guide's table.
- The cover is already at `/images/games/400/valorant.webp`.
- A test in `test-playwright/integration/guides.spec.ts` (join guide → this guide → feed), and the slug in the lists in `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and `test-playwright/screenshots.mjs`. Check the table at 375 px in `phone.spec.ts` and the 375 px screenshot.
- Verify with `.\bundle.ps1 -environment local`, then `npm test -- guides.spec.ts crawlers.spec.ts accessibility.spec.ts phone.spec.ts --timeout=120000`, then fetch the page as Googlebot from http://localhost:8020 and check the title, H1 and description.

## Timing

1. Write the guide now and launch it as soon as it's checked, with `published` and `updated` set to the real launch day.
2. When patch 14.00's notes are out, check the guide against them and the FAQ, update it where a fact changed, change its `updated`, and request indexing again.
3. Consider the Premier guide when Riot posts a Stage's dates and playoff bar.

## Distribution

The same plan as the other guides:

- request indexing in Search Console on publish
- link it from the join and make guides and the guides index
- after 4–6 weeks, check which Valorant queries reach this guide
  - If a listing query ("5 stack finder", "party finder", "duo finder") lands here, the guide is competing with the feed, so adjust its title.
  - If "rank difference" and "can X play with Y" rank but "party size" doesn't, move team and party size higher, as the Deadlock refresh did.
