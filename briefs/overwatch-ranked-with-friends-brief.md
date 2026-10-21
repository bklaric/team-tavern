# Content Brief: Overwatch team size, party size and playing ranked with friends

The seventh game guide, and the first for Overwatch. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the Marvel Rivals, Rocket League, The Finals, Deadlock and Rainbow Six Siege briefs. It takes Competitive's grouping rules and covers what a player needs to know to play ranked with friends: how big a group can be in each queue, which ranks make a narrow group and which a wide one, what a wide group costs, why a wide group of four can't queue, which of a player's three role ranks counts, what a player without a rank this season has to do first, and how PC and console players group. Overwatch is already on the site, so the guide goes out as soon as it's written. The join guide and the make guide then link here.

Prepared 2026-10-21. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, 25 Sep to 18 Oct 2026 (`now2-pq.tsv`), and 1 Jan 2025 to 18 Oct 2026 (`all2-q.tsv`)
- Google autocomplete for about 30 Overwatch seeds (`%LOCALAPPDATA%/Temp/gsc/suggest-ow.mjs` and `suggest-ow-premier.mjs`), and the Google results for "overwatch ranked with friends", "overwatch what ranks can play together", "overwatch wide group", "overwatch party size", "overwatch group restrictions", "can you play ranked with 4 people overwatch" and "overwatch console and pc ranked together", loaded on 2026-10-21 with their AI Overviews
- Blizzard's live patch notes, read month by month for grouping changes: every month of 2026 to October, and February, March, April, June, August, October and December 2025. The April and May 2024 notes, which set the current scheme, were read in full.
- the in-game Competitive panel, from two screenshots (February and August 2026), and r/Overwatch, r/OverwatchUniversity, r/Competitiveoverwatch and Blizzard forum threads from October 2025 to October 2026, read only
- the 2026-10-21 production backup, restored into a throwaway container (`%LOCALAPPDATA%/Temp/gsc/overwatch-posts.sql`, output `overwatch-posts.txt`)

## Why this topic

Chosen on 2026-10-21 from the next-guide recheck (see the blog-plans memory):

- **The pattern that works.** Guides that answer how a game's mode treats friends rank at 4–8 within days. Deadlock's leads the site with 14.5k impressions in the period, The Finals' has 3.3k.
- **Autocomplete is as full for Overwatch as for Marvel Rivals.** Ten suggestions each for "party size", "team size", "ranked with friends", "wide group", "wide groups", "rank difference", "group restrictions", "4 stack", "5 stack", "crossplay ranked", "unlock competitive", "emerald rank", and "can silver play with". "What ranks can play together" has six, "can plat play with masters" six, "can bronze play with gold" five, "can gold play with diamond" four.
- **The wide-group questions are the ones no page answers well.** "wide groups cannot queue", "wide groups cannot queue with 4 players", "wide groups cannot queue solo", "wide group meaning", "wide groups explained", "open queue wide groups cannot queue".
- **Overwatch is one of the site's four busiest feeds** by posts (2,820 player posts since 2021), but its search presence now is small: the feed had 653 impressions in the period at 8.9, all "lfg", "team finder" and "group finder". Before the relaunch the site had 7.9k Overwatch impressions. No ranked-rules query reaches the site yet, so the case rests on autocomplete, as Marvel Rivals' did.

## Template

**Recommended**: `how-to-guide`. The reader wants to know whether they can queue with a particular friend, why the game calls their group wide, and how to keep a group narrow. What sets the guide apart: the current limits, including Season 4's change that no patch note mentions; the rule turned into a table; the "two ranks apart is never narrow" shortcut that Emerald created; and the role choice that keeps a group narrow.

## Target keywords

Search Console has no Overwatch ranked volume of its own. This guide targets what a listing can't answer:

- **Primary:** overwatch ranked with friends, overwatch party size, overwatch wide group, and overwatch what ranks can play together
- **Secondary:**
  - overwatch wide groups cannot queue, wide groups cannot queue with 4 players, wide group meaning, wide groups explained, narrow group
  - overwatch group restrictions, ranked group restrictions, rank difference, max rank difference, rank gap
  - overwatch 4 stack, 4 stack comp, can you play ranked with 4 people, 5 stack ranked
  - overwatch team size, group size, ranked party size
  - can gold play with diamond, can plat play with masters, can silver play with gold
  - overwatch crossplay ranked, can console play with pc ranked
  - overwatch unlock competitive, how many wins to unlock ranked
  - "overwatch 2" forms of all of these: players still search with the 2
- **Leave to the feed:** "overwatch lfg", "team finder", "group finder", "duo finder", "lft"
- **Leave to the join guide:** OWCS, FACEIT, "overwatch teams to join", "esports teams recruiting"
- **Leave out:**
  - hero tier lists, role advice and climbing tips. That's gameplay.
  - the rank list in full, rank distribution, SR per rank, rewards. Booster and stats sites own them.
  - Stadium beyond one sentence: it has its own rank and no grouping limits in Ranked.
- **Questions:**
  - Is Overwatch 5v5 or 6v6? (People Also Ask, twice)
  - How many people can play Overwatch in a party? (PAA) Can you play Overwatch with 4 people? (PAA)
  - Can you 4 stack competitive Overwatch? (PAA) Why does Overwatch say wide groups cannot queue? (PAA)
  - What does "wide" mean in Overwatch? (PAA)
  - What ranks can play together?
  - Can you play ranked with a friend who has no rank yet?
  - Can PC and console players play ranked together?

Measure after launch instead of guessing volumes: see Distribution.

## Search intent

The intent is informational. The reader has a friend the game puts in a wide group, or won't let them queue with at all, or a group that wants to play Competitive together and needs to know who keeps it narrow.

What ranks, loaded on 2026-10-21 (Google still placed the browser in Croatia, so the order is approximate):

- **"overwatch ranked with friends":** a year-old Reddit thread, Blizzard forums (Feb 2026), Facebook, Blizzard's Team Queue post (2023) and Kotaku on it (2023), both obsolete, TikTok, Turtle Beach (Apr 2024). The AI Overview gives the Season 10 rule (narrow within 5 divisions, any Grandmaster or Champion wide), which is out of date at the top, and says "unranked or new accounts count as Bronze 5 until placements finish", which nothing supports.
- **"overwatch what ranks can play together":** the Fandom wiki, a three-year-old Reddit thread ("2 ranks at most", the old rule), the Team Queue post, HP (2024), Facebook, Blizzard forums (2023). **The AI Overview's top-rank figures match no version of the rule**: "Master: Up to 4 divisions apart. Grandmaster: Up to 3 … Champion: Up to 2" happens to be today's panel, but it says groups of four "are not allowed to queue in competitive play", which is wrong.
- **"overwatch wide group":** the wiki's Wide Group page, Reddit, a Blizzard forums bug report (Oct 2025), YouTube, GameRant (Apr 2024). The AI Overview uses the Season 10 rule and says an unranked player makes the party wide "until everyone plays solo to establish an official rank". PAA: "Can you wide match 6v6?", "What does 'wide' mean in Overwatch?", "Why does Overwatch say wide groups cannot queue?", "Is Overwatch 5v5 or 6v6?".
- **"overwatch party size":** Reddit, the wiki, Blizzard forums (2019, "Maximum players is 6"), Facebook, MMORPG.com (2021). The AI Overview answers team size only (5v5) and says nothing about party size. PAA: "Can you play Overwatch 2 with 4 people?", "Does Overwatch have 5 or 6 players?", "How many people can play Overwatch in a party?".
- **"can you play ranked with 4 people overwatch":** **the AI Overview is wrong**: "A 4-player (quad) stack is completely restricted in both Role Queue and Open Queue competitive modes." Only a wide group of four is refused in Role Queue. Its own Reddit citation says so. PAA: "Can you 4 stack competitive Overwatch?", "Can you 4 stack in 6v6 Overwatch 2?".
- **"overwatch console and pc ranked together":** Reddit, Blizzard forums (Aug 2025), Eneba (Nov 2025), a YouTube "2026 guide", GameFAQs (2023). The AI Overview says PC and console can't form a mixed Competitive party, and that a console player with mouse and keyboard enters the PC pool. The second half matches Blizzard's notes; the first is unconfirmed.

**The competitors, and what each gets wrong:**

- **Overwatch Wiki (Fandom), Competitive Play**, the page that ranks across these searches. Its table: Bronze to Diamond 5, Master and Grandmaster 2, Champion 1. That's the December 2025 rule, which the Season 4 panel widened. It has no Emerald, and its own Wide Group page copies the April 2024 text (any Grandmaster or Champion is wide), so the wiki contradicts itself.
- **clip.dor.gg, "Overwatch Rank Distribution, Tier Order & Competitive Requirements (2026)"** (updated 14 Oct 2026), the most current page. Right on Emerald (11 Aug 2026), the 50-win unlock and wide groups of four. Gives Master and Grandmaster 2, Champion 1, with no source, and says Bronze to Diamond is "not restated after Emerald". A clip-tool vendor.
- **Turtle Beach** (Apr 2024), the AI Overview's main citation: a copy of the Season 10 post. **GameRant** (Apr 2024), **Dot Esports** (2022, still in SR), **HP** (2024, vague): all before the current rules.
- **No page** covers how Emerald counts for groups, that a narrow four-stack can queue, which role's rank counts, a player without a rank this season, Stadium's lifted limits, or a worked table of who can queue with whom.

So a page that gives the current limits from Blizzard and the panel, works them out for every rank, and explains the role choice and the wide queue players actually hit has something to add to every one of these searches, and corrects the AI Overviews that rank for "4 stack" and "what ranks can play together".

Say in the first paragraph who the guide is for: a player who plays Competitive with friends, or wants a group to, and has been put in a wide group or wants to know who keeps a group narrow.

**Players hit the rules without understanding them.** From Reddit (read only; the guide doesn't link Reddit):

- "Im diamond five and can queue with my friend whos plat 3 but then we invite our emerald 5 friend and its suddenly a wide group, wtf?" (2026-10-11)
- "I'm plat 4 support and my brother is silver 2 dps but it won't let us queue together in competitive." (2026-02-17)
- "I have to play support, as it's my only role in Gold 2 that we can queue with. If I try to play DPS or tank it won't let us since my DPS is low Diamond" (2026-09-20)
- "I tried 5 stacking with my friends… it wouldnt let my friend start the queue with me in the party… with 1 person unranked." (2026-09-20)
- "if I do my placement matches, what's the highest rank I could place at and still be able to play with him without it putting us in a Wide Queue match?" (2026-10-20)
- "My main Overwatch group has ended up split up across metal ranks, so trying to queue puts us in the 'wide' queue, with longer times." (2026-07-15)
- "we are a team of 3: from bronze to plat… Wide group of 4 are not allowed in 6v6 ok, But we are 3, so why are we not allowed to play?" (2026-04-08)

The first wants the duo-to-trio jump, the second and third the role rank, the fourth the unranked rule, the fifth the table, the sixth what wide costs, and the last 6v6. The guide answers each.

## Content parameters

- **Word count:** 1,800–2,200. The join guide's Overwatch H3 gains one sentence and links here.
- **Reading level:** plain and direct, like the other guides. It says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the other guides, Markdown in `Server/Guide/`.
- **H2 sections:** 5, plus an FAQ.
- **Visuals:**
  - the game's cover, as in the other guides
  - one table: who makes a narrow group, by rank (see the outline). The specs' `getByRole("table")` is strict, so there's only one table. Three columns, four rows, checked at 375 px against `phone.spec.ts`.
  - what a wide group costs, and the queue's party sizes, as short lists, not more tables
  - no chart (see Adding TeamTavern's data)
- **FAQ:** 6 questions.
- **Naming:**
  - "Overwatch" throughout, as Blizzard has called the game since February 2026 and the seed calls it. Say once, early, "Overwatch (Overwatch 2 until February 2026)", so searchers who type the 2 recognise it. Don't use "OW2".
  - "Competitive", as Blizzard names the mode, and "ranked" where players' words are wanted, as in "play ranked with friends". Say once that they're the same.
  - "Role Queue" and "6v6 Open Queue", as the patch notes name the two Competitive queues.
  - Ranks as the game and the seed write them: "Platinum 3", "Emerald 5", "Grandmaster 1". A "rank" is Bronze, Silver and the rest; a "division" is 5 (lowest) to 1 (highest). The panel says "Skill Tier" for rank; the guide says "rank" throughout and explains "division" once.
  - "Group", as Blizzard and the panel say, and "narrow" and "wide" in Blizzard's sense, explained once. "Stack" where players' words fit ("four-stack", "five-stack").
  - Roles as the seed has them: Tank, DPS and Support. Say once that the game calls DPS "Damage".
  - "Platform" as the seed has it: PC, PlayStation, Xbox, Switch. Blizzard's "Mouse and Keyboard Pool" and "Controller Pool" once each, where crossplay needs them.

## Recommended title

**Overwatch ranked with friends: wide groups, party size and what ranks can play together**

Alternatives:

1. Overwatch wide and narrow groups: who can queue Competitive together
2. Can you play ranked with friends in Overwatch? Group size and rank limits

H1: "Overwatch team size, party size and playing ranked with friends", as the Deadlock and Marvel Rivals guides' H1s carry "team size", which the title has no room for.

Slug: `overwatch-ranked-with-friends`. It's permanent, so it names the subject, not a season.

## Meta description

Overwatch is 5v5. A group of up to five can queue ranked: within five divisions up to Diamond it's narrow, wider it's a wide group, and a wide four can't queue.

For `Guides.purs`, which the other guides keep shorter: "Overwatch team size is five. Ranked with friends: narrow and wide groups, what ranks can play together, and why a wide four can't queue."

## TL;DR draft

> - Overwatch's Competitive Role Queue is 5v5: one Tank, two DPS and two Support. A group can be any size up to five. 6v6 Open Queue takes up to six.
> - A group is narrow when everyone is close in rank: within five divisions from Bronze to Diamond, four in Master, three in Grandmaster and two in Champion. Five divisions is the same division one rank away, so Gold 3 makes a narrow group with anyone from Silver 3 to Platinum 3.
> - A wider group can still queue, as a wide group: it plays only other wide groups, waits longer, and gains and loses less rank. A wide group of four can't queue at all.
> - The rank that counts is your rank in the role you queue, so a group can often stay narrow by picking roles.
> - A friend with no rank this season plays one Competitive match before they can group. Competitive opens after 50 wins.

## Information gain

- **[UNIQUE INSIGHT] The limits as they are now.** The last patch note on grouping (9 December 2025) set Master and Grandmaster at 2 divisions and Champion at 1. The panel since Season 4 (August 2026) says 4, 3 and 2, and puts Emerald inside the Bronze-to-Diamond band. No patch note mentions the change, and no page ranking for these searches has it. State the panel's figures as what the game shows.
- **[UNIQUE INSIGHT] The rule, worked out for every rank.** Two things fall out of it that no page says plainly:
  - **Five divisions is the same division one rank away.** Every rank has five divisions, so Gold 3 reaches Silver 3 and Platinum 3, and nothing past them. It's the one sentence a player can apply without counting.
  - **Two ranks apart is never narrow.** Since Emerald sits between Platinum and Diamond, Platinum 1 and Diamond 5 are six divisions apart. So a Platinum and a Diamond player can never make a narrow group of three or more any more, and neither can Gold and Emerald, or Silver and Platinum. That's the R6 guide's "Gold and Diamond never can" for Overwatch.
- **[UNIQUE INSIGHT] Pick roles to stay narrow.** Each player has a rank per role, and the one that counts is the role they queue. A group too wide on one set of roles is often narrow on another. TeamTavern's posts show how common that is: of the player posts that give two or more role ranks, about a quarter have them two or more ranks apart (see Adding TeamTavern's data).
- **[UNIQUE INSIGHT] Adding a third player can make a narrow duo wide.** Players report it again and again: Diamond 5 with Platinum 3 is narrow, and an Emerald 5 joining makes it wide, though their rank is in between. The likely reason, from several players' tests, is that a duo is allowed a little further apart than a group of three or more. Blizzard doesn't say so (see Not confirmed), so the guide states the effect, hedged, and the advice: check the queue again whenever someone joins.
- **[CORRECTION] Four-stacks can queue.** The AI Overview for "can you play ranked with 4 people" says four-stacks are banned. Only a wide group of four is. Say it plainly, without naming Google.
- **[DATA] TeamTavern's posts.** See Adding TeamTavern's data.

## Content outline

### Introduction (~120 words)

- **Hook:** Overwatch lets a group of any rank queue Competitive together, but not on equal terms: a group whose ranks are far apart becomes a wide group, with longer queues and smaller rank changes, and a wide group of four can't queue at all. The limits changed in August with no patch note, and the top answers still give older ones.
- **Who it's for:** a player who plays Competitive with friends, or wants a group to play it with.
- **Promise:** how big a group can be, which ranks make a narrow group, what wide costs, which role's rank counts, friends without a rank, PC and console, and how to find a group in range. Link [every game's team format](/guides/join-an-esports-team#your-games-team-format) for OWCS and other games.
- The TL;DR box goes here, as "In short", like the other game guides.

### H2: Team size and party size

- **Answer first:** Overwatch's Competitive Role Queue is played five against five, with one Tank, two DPS and two Support on each team. A group can be one to five players, and the game fills the rest. 6v6 Open Queue, where anyone plays any hero, takes up to six.
- Say once that the game is Overwatch 2 until February 2026, and that Competitive and ranked are the same.
- **Every size can queue, with one exception:** a wide group of four. A narrow group of four queues like any other. One sentence, then the correction: "Some answers say four-stacks can't play Competitive; only a wide four can't."
- **Why four.** Blizzard's reason, cited to the [April 2024 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2024/04/): a wide four would need a solo player to fill its fifth place, and solo players are never put in wide matches. That one fact explains the rule better than any page that ranks.
- **Unlocking Competitive.** It opens after 50 wins, by the [May 2026 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/05/), for every player in the group. So a friend new to the game has some Quick Play to play first.
- **Out of range:** Quick Play has no rank limits, and Stadium Ranked has had none since the [June 2026 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/06/). Stadium has its own rank. One or two sentences.
- Game cover beside the first paragraph, as in the other guides.

### H2: Which ranks make a narrow group

- **Answer first:** from Bronze to Diamond, a group is narrow when everyone is within five divisions of each other. Every rank has five divisions, 5 up to 1, so five divisions is the same division one rank away: Gold 3 makes a narrow group with anyone from Silver 3 to Platinum 3. Master allows four divisions, Grandmaster three and Champion two.
- Cite the [April 2024 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2024/04/) for narrow and wide and the Bronze-to-Diamond five ("Players between Bronze and Diamond must be within five divisions of each other player in their group"). State the top-rank figures as what the game's Competitive panel shows, uncited: "Wide Groups are more than 4 Divisions apart" for Master, 3 for Grandmaster, 2 for Champion, and "Maximum width is subject to change".
- **Table: who makes a narrow group.** Columns: Your rank / Narrow with players within / For example. In the game's order:

  | Your rank | Narrow within | For example |
  |---|---|---|
  | Bronze to Diamond, Emerald included | 5 divisions: the same division one rank up or down | Gold 3: Silver 3 to Platinum 3 |
  | Master | 4 divisions | Master 5: Diamond 4 to Master 1 |
  | Grandmaster | 3 divisions | Grandmaster 4: Master 2 to Grandmaster 1 |
  | Champion | 2 divisions | Champion 3: Champion 5 to Champion 1 |

  How it's derived, for the writer: number the divisions Bronze 5 = 1 to Champion 1 = 45, with Emerald 21–25 and Diamond 26–30. A group is narrow when every pair is within the limit. The examples are chosen so they hold whichever player's limit applies in a mixed pair, which Blizzard doesn't state (see Not confirmed).
- **Emerald.** Two sentences: Emerald, added in [August 2026](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/08/), sits between Platinum and Diamond and counts like the ranks around it. Since it, players two ranks apart can never be narrow: a Platinum and a Diamond player, or a Gold and an Emerald one. Groups that were narrow before August may be wide now.
- **The edge.** The panel says groups "may be Wide when 5 Divisions apart", while the notes say "within five". Advise leaving room: a group inside one rank stays narrow when someone's rank moves, which it does after every match.
- **A third player can tip it.** Players report that a duo can be a little further apart than a group of three or more, so adding a third player between two friends can make the group wide. Say it as players' experience, hedged, without a figure, and advise checking the queue whenever someone joins.
- **Some answers are out of date.** One sentence: older guides say any Grandmaster or Champion makes a group wide, or give Master and Grandmaster two divisions; the game's own panel is the current word.

### H2: Wide groups: what they cost

- **Answer first:** a group too far apart for narrow still queues, as a wide group. It plays only against other wide groups, waits longer for a match, and gains and loses less rank the wider it is. A wide group of four can't queue.
- Cite the [April 2024 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2024/04/) ("Wide Groups have increased queue times", "The wider the group is, the less their ranks will change when winning or losing") and Blizzard's [Defense Matrix post](https://overwatch.blizzard.com/en-us/news/24061006/defense-matrix-update-season-10-and-beyond/) ("Solo players will only play with other solo players or 'Narrow Groups'"). The [December 2025 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/12/): no Challenger Score in wide matches.
- **How the game shows it.** State as what the game shows, uncited: before queuing, a "Wide Match" badge appears under the role card for the role that would make the group wide, and roles that can't be queued are locked.
- **6v6 Open Queue has no wide queue.** Blizzard's [March 2025 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/03/) say so for Competitive 6v6, so a group too far apart can't queue it at all. Its limits are wider than Role Queue's by one forum post (see Not confirmed); give them only if that post is confirmed as Blizzard's.
- **What to do:** a group that wants normal rank gains stays narrow: pick roles (next H2), or split into a narrow group and a solo or duo. A group that plays for fun can accept wide.

### H2: Role ranks, new seasons and friends without a rank

- **Answer first:** each player has a rank for Tank, DPS and Support, and the rank that counts for the group is the one for the role they queue. A friend with no rank this season plays one Competitive match before they can group with ranked players.
- Cover:
  - **Role ranks.** The panel: "Players have different Skill Tiers for each hero role". That the queued role's rank counts is from players' reports, consistent with the notes' "configuration of roles they have selected would result in a Wide Group" (April 2024). Example in the guide's own words: a Gold 2 Support whose DPS is Diamond can group narrow with Gold friends only as Support.
  - **TeamTavern's figure** on role ranks two or more ranks apart (see Adding TeamTavern's data), with the advice: before a session, compare role ranks and pick the roles that keep the group narrow.
  - **No rank yet.** Players report that a player without a rank in a role can't group with ranked players until they've played one Competitive match in it; after that their predicted rank counts. Stated as what happens, uncited. Each role needs 10 placement matches for a rank ([August 2025 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/08/)), and the prediction moves with every match, so a pair that fits after one match may not after the next.
  - **Rank resets.** Every three seasons all ranks go back to Unranked ([February 2026 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/02/)), so at the start of such a season every group has to play its first matches apart. The next is expected with Season 7, around February 2027; say "every three seasons", not a date.
- **What to do:** a friend new to Competitive plays their first match in each role they want alone, then groups.

### H2: PC, console and finding a group in range

- **PC and console. Answer first:** Competitive splits players by input, not platform: a Mouse and Keyboard pool, with every PC player, and a Controller pool, for console players on controller. Each pool keeps its own rank. A console player who switches to mouse and keyboard in Gameplay Options plays in the PC pool, on a separate rank. Cite the [August 2025 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/08/). Quick Play takes everyone. Whether a mixed PC and console group can queue Competitive with the console player on controller is unconfirmed: write only what the notes say.
- **Finding a group. Answer first:** recruit players within one rank of everyone already in the group, in the roles you queue, on the same pool, at the hours you play.
- Cover:
  - **Range first.** Use the table, and leave room: ranks move after every match, so a group inside one rank keeps fitting when a gap of exactly five doesn't.
  - **Five, not four.** A group of four that turns wide can't queue; recruit to three or five. One sentence.
  - **Roles.** A full five needs one Tank, two DPS and two Support. Name the roles you're short of, and the rank you have in each.
  - **Keep a good teammate.** After a good match, add them by their BattleTag. One sentence.
  - **From ranked to a team.** A group that plays every week and wants more can enter FACEIT's leagues; link the join guide's Overwatch section rather than repeating it.
  - One line linking the make guide for keeping a team together: "How to make an esports team".
- **The TeamTavern step:** put up a player post with your rank in each role you play, your platform, and "Ranked" ticked under Looking for. A group looking for players puts up a group post with a rank range the whole group can stay narrow with, within one rank of its members, not "Bronze to Grandmaster". Then filter the posts by rank, role and platform and message the ones that fit. Link `/games/overwatch` with the anchor "Overwatch players and groups on TeamTavern".

### FAQ (6)

1. **Is Overwatch 5v5 or 6v6?** Competitive Role Queue is 5v5: one Tank, two DPS, two Support. 6v6 Open Queue is the other Competitive queue.
2. **Can you play Competitive as a group of four?** Yes, if the group is narrow. Only a wide group of four can't queue, because its fifth place would need a solo player in a wide match.
3. **What ranks can play together in Overwatch?** Any ranks can queue Role Queue together, but only players within five divisions, the same division one rank up or down, make a narrow group up to Diamond. Master allows four, Grandmaster three, Champion two.
4. **What is a wide group?** A group too far apart in rank for narrow. It plays only other wide groups, waits longer and gains and loses less rank.
5. **Can you play ranked with a friend who has no rank?** After they've played one Competitive match in the role, using their predicted rank.
6. **Can PC and console players play ranked together?** A console player who switches to mouse and keyboard plays in the PC pool, on a separate rank. Ranks are kept per input pool.

### Conclusion (~100 words)

- Takeaways: stay within one rank to stay narrow; two ranks apart is never narrow; pick roles to fit; never queue as a wide four; a friend without a rank plays one match first.
- **Call to action:** "Overwatch players and groups on TeamTavern".

## Facts, with sources

Checked 2026-10-21.

**Blizzard's own pages. Cite these:**

- **Narrow and wide**, from the [16 April 2024 notes](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2024/04/) (Season 10):
  - "All previous Competitive Grouping Restrictions have been removed."
  - "All groups in Competitive Play are now classified as Narrow or Wide."
  - "Players between Bronze and Diamond must be within five divisions of each other player in their group to be in a Narrow Group."
  - "Narrow Groups will always be matched against other Narrow Groups or solo players."
  - "If a group has both Wide and Narrow configurations of players, the Narrow configuration will always be prioritized."
  - "Any group that does not meet the criteria is considered a Wide Group."
  - "Wide Groups of four players may not queue." / "This restriction exists so that solo players are never required to make a Wide match."
  - "Wide Groups have increased queue times and reduced match quality…"
  - "The wider the group is, the less their ranks will change when winning or losing."
  - "Players will now be informed if the configuration of roles they have selected would result in a Wide Group."
  - Its Master (3) and Grandmaster/Champion (never narrow) lines are superseded.
- **The explanation in plain words**, from the [Defense Matrix post, Season 10 and beyond](https://overwatch.blizzard.com/en-us/news/24061006/defense-matrix-update-season-10-and-beyond/) (undated, April 2024): "you'll now be able to play with your friends in Competitive … no matter their rank"; "A Wide Group is when the highest and lowest ranked players are too far apart in Skill Tier and Divisions"; "Solo players will only play with other solo players or 'Narrow Groups'"; "we won't allow a Wide Group of four players to queue because solo players do not join matches…".
- **Superseded top-rank limits**, for knowing which pages are out of date: [14 May 2024](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2024/05/): "Grand Master can now group within 3 divisions", "Champion can now group within 1 division". [9 December 2025](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/12/) (Season 20), "Grouping Restrictions": "Champion players now must be within 1 Division", "Grandmaster and Master players now must be within 2 Divisions", and "Challenger Score can't be earned in Wide matches." The Season 4 panel replaces the first two; the third stands, so cite December 2025 for Challenger Score only.
- **The rule before narrow and wide**, [13 February 2024](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2024/02/) (Season 9): Bronze–Diamond "within 2 Skill Tiers", which is the "2 ranks at most" in old Reddit answers. Don't link it from the guide.
- **6v6 Competitive has no wide queue**, [18 March 2025](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/03/): "For Competitive play, wide queue is not available".
- **Placements and input pools**, [26 August 2025](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2025/08/): "all players will need to complete their respective 10 placement matches for each role"; "PC Pool and Console Pool will now be referred to as the Mouse and Keyboard Pool and the Controller Pool"; "When you switch to the Mouse and Keyboard Pool, you will be paired with all PC platforms and any console players who are using Mouse and Keyboard inputs and your internal MMR, skill ratings, and ranks for all game modes will be separate from your statistics in the Controller Pool"; "To play on the Controller pool, you must play on a console platform"; "You can also manually switch to either pool in your Gameplay Options."
- **The 2026 reset**, [10 February 2026](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/02/) (Season 1): "All ranks in Role Queue and Open Queue in Competitive Play have been reset to Unranked", every three seasons.
- **50 wins**, [12 May 2026](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/05/): "Increased requirement to unlock Competitive Play from 20 wins to 50 wins."
- **Stadium**, [16 June 2026](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/06/) (Season 3): "Grouping Restrictions have been removed from Stadium Ranked."
- **Emerald**, [11 August 2026](https://overwatch.blizzard.com/en-us/news/patch-notes/live/2026/08/) (Season 4): "A new Rank has been added between Platinum and Diamond called Emerald." The note says nothing about grouping. The same notes add Team Drives, which group a match's winners automatically; not for the guide.
- **No change since.** The notes of August, September and October 2026 (Season 5 started 6 October) change no grouping rule. Season 6 is expected around early December 2026, at the eight-week cadence; Blizzard gives no date yet.
- **The rename.** The patch notes are titled "Overwatch 2 Retail Patch Notes" up to December 2025 and "Overwatch Retail Patch Notes" from February 2026. No Blizzard post announces the rename; the guide states it without a link.

**From the client, through players' reports.** Enough to state as what the game shows, never cited or quoted except the panel's text:

- **The Competitive panel since Season 4** ([screenshot, 2026-08-14](https://i.redd.it/ody79fjuwajh1.png); its rewards text "Emerald from 2026: Season 4 to 6" dates it): "5 Divisions per Skill Tier"; "Divisions ranked from 5 (lowest) to 1 (highest)"; "Skill Tier and Division adjust after every match"; Bronze, Silver, Gold, Platinum, Emerald and Diamond together: "Groups may be Wide when 5 Divisions apart"; Master: "Wide Groups are more than 4 Divisions apart"; Grandmaster: "Wide Groups are more than 3 Divisions apart"; Champion: "Wide Groups are more than 2 Divisions apart" (partly covered by the poster's overlay but legible); "Maximum width is subject to change"; "Players have different Skill Tiers for each hero role (Damage, Support, and Tank)."; "Solo players will always be matched with other solo players or Narrow Groups."; "Wide groups of 4 are not allowed."
- **The panel before Season 4** ([screenshot, 2026-02-15](https://i.redd.it/raoycfdpaljg1.png)): Bronze to Diamond "Groups may be Wide when 5 Divisions apart"; Master and Grandmaster "Groups may be Wide when 2 Divisions apart"; Champion "Wide Groups are more than 1 Division apart". It matches the December 2025 notes, so the change came with Season 4. A forum post of 2026-09-09 gives the same new figures ("dropping to 4 in masters and 3 in gm").
- **The queued role's rank counts**, from four reports (2026-03-01, 2026-09-20, 2026-10-20, and the February thread): a player is narrow on the role whose rank fits and wide on another.
- **The "Wide Match" badge** under the role card that would make the group wide, and "Placement Progress: 0/10" on a role in placements (screenshots of 2026-08-20). A locked role card where a role can't be queued, from players' descriptions.
- **A player without a rank can't group** with ranked players until they've played one Competitive match (2025-12-22, 2026-09-20, 2026-10-02, 2026-10-20), and a predicted rank after one match can split friends ("Got put in bronze 3 after one match and can't play with friends", 2026-09-13).
- **Wide groups of 2, 3 and 5 queue in Role Queue** (2026-04-08), and a wide match has no further rank limit (2026-10-20).
- **PC and console split in Competitive**, from players (2026-02-17, 2026-04-28), and a PC player who joined a console friend got a fresh rank there (2026-10-03).

**Not confirmed, or sources disagree.** The guide handles each like this:

- **The duo allowance.** Five reports from December 2025 to October 2026 say a duo stays narrow up to about seven divisions apart while a group of three or more is held to five ("Its 7 divisions difference for duo but only 5 for trio and more", 2026-10-11), and several describe a narrow duo turning wide when a third player joins. Neither the panel nor the notes say so. The guide states the effect, hedged as players' experience, and gives no figure.
- **Exactly five apart.** The notes say "within five" (narrow at five); the panel says "may be Wide when 5 Divisions apart". Players disagree, and some blame hidden MMR near the edge. The guide says five is the limit and advises leaving room.
- **Mixed pairs at the top.** Whether a Master with a Grandmaster is held to Master's four or Grandmaster's three is unstated. The table's examples hold either way; the guide doesn't claim one.
- **6v6 Open Queue limits.** A forum post by "Athena" of [21 March 2025](https://us.forums.blizzard.com/en/overwatch/t/grouping-up-in-6v6-open-queue-competitive-play/957097) gives Bronze–Diamond within ten divisions, Master and Grandmaster five, Champion three, and no wide groups. Settled while writing: the topic's JSON marks Athena as staff in the `community-manager` group, titled "Overwatch Community", so the guide gives the figures with that link, dated March 2025, since they predate Emerald and Season 4. Players' 2026 reports agree that wide groups can't queue 6v6 and disagree on the limit.
- **6v6 Open Queue in 2026.** Blizzard announced it in March 2025 as temporarily replacing 5v5 Open Queue, and no note makes it permanent, but every 2026 note balances "6v6 Open Queue", the February 2026 reset names Competitive's "Open Queue", and players queued 6v6 Competitive in April and October 2026. The guide calls Competitive's Open Queue 6v6; check it each season.
- **Mixed PC and console groups on controller.** No current Blizzard page says what happens when a PC player groups with a console player on controller. Players say they can't play Competitive together; one reports a PC player getting a console rank. The guide gives the input-pool rule from the notes and no more.
- **What "50 wins" counts.** The note doesn't say which modes. The guide says "50 wins".
- **Any error text.** Nobody quotes a dialog beyond the panel, the badge and the lock. The guide quotes none.
- **Solo players in wide matches.** The panel says never; a few players say they were. Leave it out.

## Adding TeamTavern's data

**Counted while writing**, from the 2026-10-21 production backup (`overwatch-posts.sql`, output `overwatch-posts.txt`):

- 2,820 player, 246 group and 49 community posts, from February 2021 to October 2026. Only 8 player posts are from after the relaunch, so the figures are about who has posted since 2021, and the guide says so.
- Ranked is ticked on 84% of player posts (2,378) and 81% of group posts (200).
- Player posts by platform: PC 2,112, PlayStation 488, Xbox 193, Switch 26.
- Rank per role: 1,530 player posts give a Tank rank, 1,699 DPS, 2,019 Support.
- **Role ranks apart:** of the 1,655 player posts with two or more role ranks, 395 (24%) have their highest and lowest two or more ranks apart, which can never make a narrow group with the same friends on every role.
- **Group ranges:** 208 group posts give a rank range; 63 of them (30%) ask, in at least one role, for a range wider than one rank, which a group of three or more can't keep narrow, and 21 span three ranks or more.
- **Caveat for the writer:** ranks carried over from the old site are whole ranks, stored as division 3 (5,231 of 5,254 rank values), so only rank-level figures are meaningful, not divisions. Emerald didn't exist when most posts were made. The two figures above are counted at the rank level and hold either way.

Use the role-rank figure in the role H2 and the group-range figure in the finding H2, each with "TeamTavern's posts, 2021–2026, counted 21 October 2026", as the join guide does. No chart: neither figure gains from one, and the join guide already charts Overwatch's roles.

## Internal links

- **Links from this guide:**
  1. `/games/overwatch`, twice: in the finding H2 and in the conclusion. Anchor: "Overwatch players and groups on TeamTavern".
  2. The join guide, from the introduction ("every game's team format") and the finding H2 (its Overwatch section, `/guides/join-an-esports-team#overwatch`, for FACEIT).
  3. The make guide, from the finding H2. Anchor: "How to make an esports team".
- **Links to this guide:**
  1. The join guide: in its "Overwatch" H3, one sentence before the feed link: most groups start in Competitive, where a group of up to five queues together and stays narrow within five divisions up to Diamond, and a link here with the anchor "Overwatch team size, party size and playing ranked with friends", as its Deadlock H3 links its guide by H1.
  2. The make guide: a row in its format table, before the OWCS row: **Overwatch: Competitive** | Nobody: a group of up to 5 queues Role Queue together, and the game fills empty places | Narrow within five divisions up to Diamond, fewer above; a wider group plays only wide groups, and a wide group of 4 can't queue.
  3. The `/guides` index, and the guide's `games` list naming `overwatch`, so it shows on the guides list with its game.
- Editing the join and make guides adds a fact, so their `updated` changes too.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, and logo-512.png as `image`, as the other guides have. BreadcrumbList Home > Guides > the H1. `datePublished` and `dateModified`. No Person.
- Every rule the guide cites links to Blizzard's own page, on the words that name it, once per source per section. What comes from the client and players' reports (the panel's top-rank limits, the badge, the queued role's rank, the unranked rule, the duo effect) is stated as what the game shows, without a link. Reddit and community Discord servers are neither linked nor named.
- No "when we played" stories. Nobody here plays the game.

## Keeping it current

An Overwatch season lasts about eight weeks, and grouping changed in April 2024, May 2024, December 2025 and, silently, August 2026. The panel says "Maximum width is subject to change".

- **Season 6, expected early December 2026.** Read its notes for "Grouping", "Narrow", "Wide", "division" and "Competitive", and look for a newer screenshot of the panel, since the last change came without a note.
- **Season 7, expected about February 2027,** is the next reset season by the three-season pattern: placements and the unranked rule peak then.
- **Each month's live patch notes**, with the same words.

Change `updated` only when a fact changes.

## Implementation

- `Server/Guide/overwatch-ranked-with-friends.md`, imported as text in `Guides.js`, and an entry in `Guides.purs`: slug, heading, title, description, published, updated, and `games: [ { handle: "overwatch", title: "Overwatch" } ]`.
- The sentence in the join guide's Overwatch H3 and the row in the make guide's table.
- The cover is already at `/images/games/400/overwatch.webp`.
- A test in `test-playwright/integration/guides.spec.ts` (join guide → this guide → feed), and the slug in the lists in `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and `test-playwright/screenshots.mjs`. Check the table at 375 px in `phone.spec.ts` and the 375 px screenshot.
- Verify with `.\bundle.ps1 -environment local`, then `npm test -- guides.spec.ts crawlers.spec.ts accessibility.spec.ts phone.spec.ts --timeout=120000`, then fetch the page as Googlebot from http://localhost:8020 and check the title, H1 and description.

## Timing

1. Write the guide now and launch it as soon as it's checked, with `published` and `updated` set to the real launch day. Launching about six weeks before Season 6 lets Google index it before the start-of-season questions.
2. When Season 6's notes are out, check the guide against them and the panel, update it where a fact changed, change its `updated`, and request indexing again.

## Distribution

The same plan as the other guides:

- request indexing in Search Console on publish
- link it from the join and make guides and the guides index
- after 4–6 weeks, check which Overwatch queries reach this guide. If a listing query ("lfg", "group finder", "duo finder") lands here, the guide is competing with the feed, so adjust its title. If "wide group" queries rank but "party size" ones don't, move team and party size higher, as the Deadlock refresh did.
