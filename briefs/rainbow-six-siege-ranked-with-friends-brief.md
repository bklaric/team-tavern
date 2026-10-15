# Content Brief: Playing ranked with friends in Rainbow Six Siege, and finding a stack in range

The fifth game guide, and a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the Marvel Rivals, Rocket League, The Finals and Deadlock briefs. It takes Siege's Ranked 3.0 squad restrictions and covers what a player needs to know to play ranked with friends: which ranks can queue together, how the game counts the gap, why friends who play their placements together can't queue afterwards, how a mixed squad is matched, what crossplay does to a console player's rank, what every friend needs before ranked opens, and how to find a stack inside the gap. Rainbow Six Siege is already on the site, and its feed is the site's strongest, so the guide goes out as soon as it's written. The join and make guides then get a line and a row that link here.

Prepared 2026-10-15. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, Jun 2025 to 14 Oct 2026, and 29 Sep to 14 Oct 2026 for the guides
- Google autocomplete for 25 Siege ranked seeds, and the current Google results for "r6 what ranks can play together", "r6 ranked requirements" and "r6 ranked crossplay", loaded on 2026-10-15
- Ubisoft's news posts, season pages, roadmap and help articles, read in full on 2026-10-15, each with its date below
- r/Rainbow6 threads from July to October 2026, read only
- TeamTavern's Siege posts in the development database, a copy of the old site's posts to 2026-09-14. The queries are in `%LOCALAPPDATA%/Temp/gsc/r6-posts.sql`.

## Why this topic

Chosen on 2026-10-15 from the next-guide check (see the blog-plans memory):

- **The pattern that works.** The guides that answer how a game's mode treats friends ranked within days: Deadlock's took 3,747 impressions at position 6.6 in eight days, The Finals' 1,817 at 4.5, on queries such as "deadlock ranked party size" and "what ranks can play together".
- **Siege is the site's strongest game.** `/games/rainbow-six-siege` had 2,996 impressions and 107 clicks at position 6.4 since the relaunch, more clicks than any page but the home page. A guide here can send readers to a feed that already works.
- **The rules changed in June 2026 and the results haven't caught up.** Ranked 3.0 brought back squad restrictions after three and a half years without them. Pages from January 2026 still rank and say there are none (see Search intent).
- **Autocomplete is full.** Every one of these has ten suggestions: "r6 rank restrictions", "r6 ranked 3.0", "r6 ranked requirements", "r6 placement matches", "how does r6 ranked work", "siege x ranked". "r6 cant play ranked with friends", "r6 ranked 3.0 can't play with friends" and "r6 ranked crossplay" (seven) are there too.

Rejected alternatives for Siege: a Siege Cup guide (the join and make guides already carry its facts, and its queries are "siege cup team finder", 86 impressions, which the feed owns), and a ranks-explained guide (booster sites and the wiki own it, and it's gameplay, not people).

## Template

**Recommended**: `how-to-guide`. The reader wants to know whether they can queue with a particular friend, why the game refused, and how to keep a stack together. What sets the guide apart is the rule turned into a table of who can play with whom, the division arithmetic behind the in-game message, and the two traps (placements and the separate PC rank) that no ranking page explains.

## Target keywords

Search Console has no ranked queries for Siege: its 14,560 Siege impressions in sixteen months are all listing queries ("r6 lfg", "r6 stack finder", "r6 lft"), and `/games/rainbow-six-siege` should keep every one of them. This guide targets what a listing can't answer:

- **Primary:** r6 ranked with friends, and r6 what ranks can play together
- **Secondary:**
  - r6 rank restrictions, r6 ranked party restrictions, r6 ranked squad restrictions, r6 ranked 3.0 restrictions
  - r6 cant play ranked with friends, and the game's message, "high rank disparity in squad"
  - r6 ranked requirements, and r6 ranked level requirement
  - r6 placement matches, and r6 placement matches ranked 3.0
  - r6 ranked crossplay, and is r6 ranked crossplay with pc
  - how does r6 ranked matchmaking work
- **Leave to the feed:** "r6 lfg", "stack finder", "team finder", "squad finder", "r6 lft", "r6 mates"
- **Leave out:**
  - operators, maps, the ranked map pool and climbing tips. That's gameplay, and it changes every season.
  - the full rank list, rank distribution and Competitive Rewards. Booster sites and the wiki own them.
  - the Legend Division beyond one sentence: it's solo queue, so it isn't about friends.
  - pro esports. The join guide covers the Challenger Series.
- **Questions:**
  - What ranks can play together in Siege? (autocomplete, and People Also Ask's "Can gold 1 and plat 3 play together?", "Can Bronze play with plat R6?")
  - Why can't I play ranked with my friend?
  - Can you play ranked with friends in placements?
  - Can a four-stack play ranked?
  - Can PC and console players play ranked together?

Measure after launch instead of guessing volumes: see Distribution.

## Search intent

The intent is informational. The reader has a friend the game won't let them queue with, or wants to know before they try.

The results for "r6 what ranks can play together", loaded on 2026-10-15:

1. **Ubisoft's [Ranked 3.0 Update](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/5fzYRZKVVHqqRkkv3m4MyF/ranked-30-update)** (May 2026) is first, and Google's AI Overview follows it. It states the rule in two sentences, but not how the game counts it, where the edge is, or who that leaves you able to play with.
2. **A Reddit thread from seven months ago** asking where the caps are, from before Ubisoft published them.
3. **Esports Insider** (5 Jan 2026): "there are no restrictions on which ranks can play together". Outdated since June.
4. **GameTree** (17 Jan 2026): "Rainbow Six Siege has no rank restrictions for who can queue together in Ranked mode". Outdated.
5. **GG Chest** (16 Apr 2026): restrictions "at higher tiers", without figures.
6. **BuyBoosting** (24 Sep 2026), a booster's rank list.
7. **A Facebook answer**: "within 4 ranks of each other". Wrong.

For "r6 ranked requirements" the first result is a two-year-old Reddit thread, then the Fandom wiki, then Ubisoft's Ranked 3.0 post. A Facebook answer says level 30. The AI Overview lists the requirements, then mixes the Legend Division's (Platinum last season, current Champion, level 200) into a heading with Siege Cup, as if Siege Cup asked for them. For "r6 ranked crossplay" Ubisoft's help article is second and the AI Overview is right. One AI Overview for friends who can't queue suggests "Quick Match, Standard" instead. Standard was replaced by Unranked in Siege X.

So a page that gives the current rule from Ubisoft, works it out for every rank, and explains the refusals players actually hit has something to add to every one of these searches, with only Ubisoft's own post ahead of it.

Say in the first paragraph who the guide is for: a player who plays ranked with friends, or wants to, and has been refused or wants to know who they can queue with.

**Players hit the rule without understanding it.** From r/Rainbow6 (read only; the guide doesn't link Reddit):

- "If I'm gold 2 what is the lowest rank my friend can play with me? I can't see a definitive list anywhere." (2026-09-07)
- "my brother is Plat V and im diamond V the restrictions for diamond is +- 2 full ranks so im supposed to be able to play with him but i cant" (2026-10-02)
- "im 20 mmr on plat5 and my friend is 80mmr on bronze 5, and its withing 15 ranks which ubisoft states", answered "it isn't within 15 ranks it is exactly 15 ranks" (2026-08-17)
- "We jumped into ranked, and we won all 5 of our placement matches, however I got bronze and he got plat, we can now no longer queue together." (2026-09-03)
- "I cant play with my bronze friend because of rank disparity and I can't play with my PC friends because I got placed in bronze as it was my first season in PC matchmaking." (2026-10-07)

The first wants the table, the second and third the edge, the fourth the placement trap and the fifth the separate PC rank. The guide answers each.

## Content parameters

- **Word count:** 1,700–2,100. The join guide's Siege section gains one or two sentences and links here.
- **Reading level:** plain and direct, like the other guides. It says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the other guides, Markdown in `Server/Guide/`.
- **H2 sections:** 7, plus an FAQ.
- **Visuals:**
  - the game's cover, as in the other guides
  - one table: who can queue with whom, by rank (see the outline). The specs' `getByRole("table")` is strict, so there's only one table. Three columns, eight rows, checked at 375 px against `phone.spec.ts`.
  - the unlock requirements as a short list, not a second table
  - no chart at launch (see Adding TeamTavern's data)
- **FAQ:** 5 questions.
- **Naming:**
  - "Rainbow Six Siege" in the title, H1 and first sentence, then "Siege", as the site and the other guides have it. "R6" only in the title tag's keyword and where quoting what players search.
  - "Ranked 3.0" by name, since players search for it, and "since June 2026" where its age matters. Name no season in the body; the guide's `updated` date carries when it was true.
  - Ranks and divisions as the game writes them: "Platinum V", "Diamond I". A "rank" is Copper, Bronze and the rest, a "division" is V to I. Ubisoft says "full Ranks" and the game's message says "15 ranks" for divisions; say "divisions" for what the game counts and explain the message once.
  - "Squad", as the game and Ubisoft say, and "stack" for a squad that plays together regularly, as players say. "Five-stack" as the join guide writes it.
  - "Unranked" and "Quick Match", never "Standard".
  - "Platform" options as the seed has them: PC, PlayStation, Xbox.

## Recommended title

**R6 ranked with friends: what ranks can play together in Siege, and how to find a stack**

Alternatives:

1. Rainbow Six Siege ranked with friends: Ranked 3.0 squad restrictions explained
2. What ranks can play together in Rainbow Six Siege, and why your friend can't queue with you

H1: "How to play ranked with friends in Rainbow Six Siege".

Slug: `rainbow-six-siege-ranked-with-friends`. It's permanent, so it names the subject, not "Ranked 3.0", which Ubisoft will one day replace, and not "finder".

## Meta description

Since Ranked 3.0, a squad must be within three full ranks, two with a Diamond or Champion: who can queue together in Rainbow Six Siege, and how to find a stack in range.

## TL;DR draft

> - A ranked squad is up to five players, and the game fills the empty places.
> - Since Ranked 3.0 in June 2026, squadmates from Copper to Emerald must be within three full ranks of each other, and a squad with a Diamond or Champion player within two.
> - The game counts divisions, so two full ranks apart always works, one with a Diamond or Champion, and one rank further depends on your divisions. Exactly three full ranks apart, such as Platinum V and Bronze V, is refused.
> - Placement matches have no restriction, but friends who place together can come out too far apart to queue afterwards.
> - A squad with a rank gap is matched closer to its highest player, and Rank Points follow.
> - A console player who queues with PC friends plays in the PC pool with a separate PC rank.

## Information gain

- **[UNIQUE INSIGHT] The rule, worked out for every rank.** Ubisoft gives two sentences; players ask "if I'm gold 2 what is the lowest rank my friend can play with me". The table answers it, and two things fall out of it that no page says:
  - Two full ranks apart always fits, one when a Diamond or Champion is in the squad, whatever the divisions: the widest such pair is 14 divisions apart (Gold V and Emerald I), or 9 (Emerald V and Diamond I). One rank further fits only at the right divisions.
  - The stricter limit wins. Gold is three full ranks from Diamond, inside Gold's own limit, but the Diamond player is limited to two, so Gold and Diamond never queue together. The same holds for Platinum and Champion.
- **[UNIQUE INSIGHT] The message explained.** The game says "Squad members must be within 15 ranks to play Ranked." It means divisions: three full ranks of five divisions each. Players report that exactly 15 apart is refused, so "within" means fewer than 15 (fewer than 10 with a Diamond or Champion).
- **[UNIQUE INSIGHT] The two traps.** Placement has no restriction, so friends queue their five placement matches together and can then land too far apart to queue again. And a console player who turns crossplay on to play with PC friends has a separate PC rank, which starts with its own placements, so the first season in the PC pool can leave them out of range of the PC friends they joined it for.
- **[CORRECTION] Outdated pages.** Esports Insider and GameTree say Siege has no squad restrictions, which was true from December 2022 until June 2026. A Facebook answer says four ranks. An AI Overview suggests Standard, which no longer exists.
- **[DATA] TeamTavern's posts.** 85% of Siege player posts and 76% of group posts on TeamTavern ask for ranked, and about a quarter of group posts ask for a rank range wider than Ranked 3.0 lets a squad queue. See Adding TeamTavern's data.

## Content outline

### Introduction (~120 words)

- **Hook:** For three and a half years anyone could queue ranked with anyone in Siege. Since Ranked 3.0 in June 2026 the game refuses a squad that's too far apart with "High rank disparity in squad", and several of the pages that rank for the question still say there's no limit.
- **Who it's for:** a player who plays ranked with friends, or wants a stack to play it with.
- **Promise:** which ranks can queue together, how the game counts the gap, the placement and crossplay traps, how a mixed squad is matched, what every friend needs before ranked opens, and how to find a stack in range. Link [every game's team format](/guides/join-an-esports-team#your-games-team-format) for other games.
- The TL;DR box goes here, as "In short", like the other game guides.

### H2: Which ranks can queue ranked together

- **Answer first:** squadmates from Copper to Emerald must be within three full ranks of each other, and a squad with a Diamond or Champion player within two. Up to five players queue together, and the game fills the rest.
- Cite Ubisoft's Ranked 3.0 post for the rule, on the words that name it.
- **Table: who you can queue with.** Columns: Your rank / Always / Only at close divisions. In the game's order:

  | Your rank | Always | Only at close divisions |
  |---|---|---|
  | Copper | Copper to Silver | Gold |
  | Bronze | Copper to Gold | Platinum |
  | Silver | Copper to Platinum | Emerald |
  | Gold | Bronze to Emerald | Copper |
  | Platinum | Silver to Emerald | Bronze, Diamond |
  | Emerald | Gold to Diamond | Silver, Champion |
  | Diamond | Emerald to Champion | Platinum |
  | Champion | Diamond to Champion | Emerald |

  How it's derived, for the writer: number the ranks Copper 1 to Champion 8. Two ranks up to Emerald are "always" if they're at most 2 apart and "close divisions" if 3 apart; a pair with a Diamond or Champion is "always" at 1 apart and "close divisions" at 2. Anything further is never. The "Always" column holds whether the game's edge is "fewer than" or "at most", so it's safe either way.
- One sentence under the table: a rank missing from a row can't queue with you at any division, so a Gold player and a Diamond player can't play ranked together even though they're three ranks apart, because the Diamond player's limit is two.
- **Squad size.** Up to five. Ubisoft lists no restriction on any size, and parties of four with one solo player are common. Ubisoft says matchmaking uses rank, squad size, and time and location.
- **Out of range:** Unranked and Quick Match have no restriction. One sentence.
- Game cover beside the first paragraph, as in the other guides.

### H2: How the game counts the gap

- **Answer first:** in divisions. Every rank has five, V up to I, so three full ranks are 15 divisions and two are 10.
- Cover:
  - **The message.** A squad out of range sees "High rank disparity in squad" on the Ranked tile, and "Squad members must be within 15 ranks to play Ranked." The "ranks" in the message are divisions. State it as what the game shows, uncited.
  - **The edge.** Exactly 15 divisions apart is refused: Platinum V and Bronze V can't queue, and neither can Diamond V and Platinum V, exactly 10 apart. So count the divisions between the squad's highest and lowest player, and keep it under 15, or under 10 with a Diamond or Champion. Stated as what players run into, uncited (see "Not confirmed").
  - **A worked example.** Gold II, counting down 14 divisions, can play with Copper I but not Copper II. Counting up, it reaches all of Emerald, but no Diamond: Diamond V is 12 divisions above Gold II, past the Diamond player's 10. Keep it to this one example.
  - **The whole squad.** The message speaks of squad members, so in a five-stack the gap that counts is between its highest and lowest player.
  - **Champion has divisions now.** Ranked 3.0 gave Champion five divisions like the other ranks, so a Champion I and a Diamond V are 9 apart and can queue.

### H2: Placements: play them together, then check the gap

- **Answer first:** the five placement matches at the start of each season have no squad restriction, so friends of any rank can place together. But their starting ranks can come out further apart than the game allows afterwards.
- Cover:
  - **How a season starts.** Five placement matches with increased Rank Point gains, then a starting rank that also depends on the season before. Cite Ubisoft's Ranked 3.0 post and its help article on rank.
  - **Why friends place apart.** Each player's starting rank carries their own last season, so a duo that wins all five together can still land Bronze and Platinum. One sentence that players see this, uncited.
  - **What to do.** Expect it, and if you land out of range, play Unranked together until your ranks close, or each climb alone until they do.
  - Don't give the placement RP figures or the reset size: they come from a developer presentation, not a written Ubisoft note (see "Not confirmed").

### H2: How a squad with a rank gap is matched

- **Answer first:** the game balances squad by squad, and a squad with a big gap is weighted towards its highest player.
- Cover:
  - **Ubisoft's wording:** "When squads contain large Rank gaps, the system weights the team closer to the higher-ranked player."
  - **What it does to Rank Points:** beating stronger teams gives more, beating weaker ones less, and losing to lower-ranked teams costs more. So a squad with a Platinum and a Silver plays lobbies nearer Platinum, and the Silver player's wins there are worth more, and losses at the lower end cost the Platinum player more.
  - **No hidden MMR.** Since Ranked 3.0 the visible rank is what matches you. One sentence: what you see is what the gap is measured on.
- **Close with the advice:** the closer a stack's ranks, the fairer its lobbies for everyone in it.

### H2: Crossplay: PC and console friends

- **Answer first:** console players can queue ranked with PC players by turning crossplay on, but the whole squad then plays in the PC pool, with the console player on a separate PC rank.
- Cover, citing Ubisoft's crossplay help article and the Collision Point season page:
  - Crossplay is always on for PC and off by default on console. A console player turns it on to join the PC pool for ranked.
  - Any squad with a PC player plays in the PC pool. PC players can't join the console pools.
  - A console player's PC rank is separate from their console rank, and the restriction is measured on it. That's the trap: a first season in the PC pool can place a console player far from their PC friends. One sentence that players see this, uncited.
  - PlayStation and Xbox players can squad up through Ubisoft Connect. State it, citing the Solar Raid season page; the current wording of the console setting is unconfirmed, so don't name its options.
  - Mouse and keyboard on console is allowed only in the PC pool. One sentence, citing the System Override season page.
  - Text chat is unavailable on consoles, so a mixed squad talks by voice.

### H2: What every friend needs before ranked opens

- **Answer first:** each player in the squad needs a paid edition, Clearance Level 50 and 10 Attackers and 10 Defenders unlocked, plus account security that depends on the platform. Free access doesn't include ranked.
- A list, from Ubisoft's competitive modes requirements article:
  - an Elite or Ultimate Edition. PC Game Pass gives the Elite Edition.
  - Clearance Level 50
  - 10 Attackers and 10 Defenders unlocked
  - 2-Step verification on the Ubisoft account
  - on PC, a phone number on the Ubisoft account
  - on console, the Ubisoft account linked to the platform account, and an active PlayStation Plus or Xbox Game Pass subscription
- **Reputation.** A Dishonorable standing locks a player out of Ranked; Disruptive locks them out of Siege Cup and Ranked rewards. One sentence, citing the Prep Phase season page.
- **The Legend Division.** One sentence: the playlist above Champion is solo queue only, so a stack of Champions plays the ordinary ranked playlist together. Cite the Split Fire season page. Don't give its entry requirements beyond "current Champion and Platinum last season".
- **Why this matters for a stack:** a new friend on free access, or below level 50, can't join a ranked squad, so check before you plan a night around them.

### H2: Finding a stack in range

- **Answer first:** recruit players within two full ranks of everyone already in the stack, one with a Diamond or Champion, on a platform that shares your pool, at the hours you play.
- Cover:
  - **Range first.** Use the table: the "Always" column is the safe range for a regular stack, since ranks move during a season and a gap that fits today may not next week.
  - **Platform.** PC players recruit from PC and console players with crossplay on; console players who want to stay in the console pool recruit console players.
  - **Roles.** A stack wants entry, support, roamer and anchor covered, as the seed's Role field has them, and someone to make the calls. One sentence; link the join guide's role chart rather than repeating it.
  - **Keep a good random.** After a good match, invite them through Ubisoft Connect, which is how Siege adds friends across platforms. There is no in-game squad finder yet; Ubisoft's roadmap lists a new social hub for inviting players in-game "Scheduled" for the next season. State the roadmap item without promising it.
  - **The official Discord.** The official Rainbow 6 Discord is where Ubisoft's community points players looking for a squad. One sentence, naming it; link only if its invite is on a Ubisoft page.
  - **From ranked to Siege Cup.** A stack that plays together every week can enter Siege Cup, the in-game tournament for five-stacks, every weekend, with the same requirements as ranked. Its roster locks at registration, so register the five who'll show up. Link the join guide's Siege section and the make guide's, rather than repeating them.
  - One line linking the make guide for keeping a team together: "How to make an esports team".
- **The TeamTavern step:** put up a player post with your rank, roles and platform, and "Ranked" ticked under Looking for. A stack looking for players puts up a group post with a rank range that the whole stack can queue with, at most two full ranks either side of its members (one with a Diamond or Champion), not "Copper to Champion". Then filter the posts by rank and platform and message the ones that fit.

### FAQ (5)

1. **What ranks can play together in Rainbow Six Siege ranked?** Squadmates from Copper to Emerald within three full ranks, fewer than 15 divisions apart; with a Diamond or Champion in the squad, within two, fewer than 10. Two full ranks apart always works, one with a Diamond or Champion.
2. **Why can't I play ranked with my friend?** Most often the rank gap: the Ranked tile says "High rank disparity in squad". Also check that both of you meet the requirements (paid edition, level 50, 2-Step verification), and, if one of you is on console with crossplay on, that it's the PC rank that's being compared.
3. **Can you play placement matches with friends?** Yes. Placements have no squad restriction, but you can come out of them too far apart to queue together.
4. **Can a four-stack play ranked?** Yes. A squad of any size up to five can queue, and the game fills the empty places.
5. **Can PC and console players play ranked together?** Yes, if the console player turns crossplay on. The squad plays in the PC pool, and the console player has a separate PC rank there.

### Conclusion (~100 words)

- Takeaways: keep the stack within two full ranks to be safe, place together but check the gap after, expect lobbies near your best player, and know which pool and rank a console friend is in.
- **Call to action:** "Rainbow Six Siege players and groups on TeamTavern".

## Facts, with sources

Checked 2026-10-15.

**Ubisoft's own pages. Cite these:**

- **The squad restriction, placements, matching and Champion's divisions**, from Ubisoft's [Ranked 3.0 Update](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/5fzYRZKVVHqqRkkv3m4MyF/ranked-30-update), May 2026 (Google shows 17 May, the en-us page 18 May):
  - "Ranked 3.0 removes hidden MMR, so that your skill is truly your rank. It also introduces Placement Matches, a new Competitive Rewards track, squad restrictions, and changes to the Champion rank."
  - "To maintain competitive integrity, Ranked 3.0 introduces Squad Restrictions. Copper to Emerald can queue with teammates within 3 full Ranks of their own Rank. Diamond and Champion players are limited to teammates within 2 full Ranks."; "These restrictions are designed to reduce extreme Rank disparities while preserving matchmaking balance."
  - "Placement Matches do not include Squad Restrictions, allowing players to queue freely with friends regardless of Rank."
  - "At the start of each season, players will complete 5 Placement Matches before receiving their starting Rank. These matches grant increased Rank Point gains to help ensure players reach the level they were at in the previous season more quickly."
  - "Team balancing is calculated at the squad level first. When squads contain large Rank gaps, the system weights the team closer to the higher-ranked player to better reflect the squad's overall strength. This balancing directly impacts RP gains and losses: Defeating stronger teams grants additional RP / Beating lower-ranked teams grants reduced RP / Losing to lower-ranked teams results in larger RP penalties"
  - "Champion Rank now includes 5 divisions, matching the rest of the Ranked ladder."
  - Demotion Shield: "If a player would normally derank after a loss, the system can instead leave them at 0 RP for that division." Not for the guide.
- **Ranked 3.0's launch**, from the [Operation System Override](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/seasons/systemoverride) season page, launched 2 June 2026: "Squadmates need to be within three ranks of each other, or two for Diamond and Champion ranks."; "all ranks will be reset and players must complete 5 placement matches … your performance in previous seasons also has an influence."; matchmaking uses "your rank, squad size, and time and location"; M&K on console "will be allowed in the PC cross play pool, but will still be forbidden in other pools."
- **The Legend Division**, from the [Operation Split Fire](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/seasons/splitfire) season page, launched 1 September 2026: "The new SoloQ-only Legend Division playlist … Only players who meet the criteria, including current Champion rank and Platinum rank last season, a positive reputation standing, and completed security requirements, will have access"; it "opens mid-season".
- **The roadmap**, Ubisoft's [Siege roadmap](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/roadmap), last updated 23 September 2026: Season 4 "NEW SOCIAL HUB – Invite friends and players to your squad more easily, directly in-game", Scheduled. The Y10S2 entry: "Standard mode is removed from playlists and Unranked is now back".
- **Requirements**, from Ubisoft Help's [competitive modes requirements](https://www.ubisoft.com/en-us/help/rainbow-six-siege/gameplay/article/competitive-modes-requirements-for-rainbow-six-siege/000063791) (no date shown; it mentions the Legend Division, so it's current): "to access competitive modes, such as Ranked mode and Siege Cup, you will need to: Own an Elite or Ultimate Edition of the game … Have an in-game Clearance of at least level 50. Have at least 10 Attacker and 10 Defender Operators unlocked. On console platforms: Secure your Ubisoft account by activating 2-Step verification. Link your Ubisoft account to your platform account. Have active PlayStation Plus subscription or a Xbox Game Pass subscription. On PC platforms: Secure your Ubisoft account by activating 2-Step verification. Associate your phone number with your Ubisoft account."
- **Free access**, from Ubisoft's [Free Access](https://www.ubisoft.com/en-us/game/rainbow-six/siege/free-access) page: "Certain features like Ranked playlists, Siege Cup, additional operators, and cosmetics require payment to unlock."; "Upgrading to a premium offer grants access to more operators and unlocks Ranked and Siege Cup modes as players progress to level 50."
- **PC Game Pass**, from Ubisoft Help's PC Game Pass article (updated 12 October 2026, linked from the Siege Cup article): "Elite Edition … Competitive modes (Ranked mode and Siege Cup)". Confirm the article's own URL before linking.
- **Rank and the soft reset**, from Ubisoft Help's [Information about Rank](https://www.ubisoft.com/en-us/help/rainbow-six-siege/gameplay/article/information-about-rank-in-rainbow-six-siege/000063203), updated 10 August 2026: "At the beginning of each new Season, your Rank will be soft reset. It means your Rank will depend on the previous Season and your placement for your first five matches of the Season."; "your Rank is different when joining the PC matchmaking or the console matchmaking."
- **Crossplay**, from Ubisoft Help's [Crossplay and cross-progression](https://www.ubisoft.com/en-us/help/rainbow-six-siege/gameplay/article/crossplay-and-cross-progression-in-rainbow-six-siege/000097631), updated 3 August 2026: "Crossplay is activated by default on PC and can not be deactivated, and is deactivated by default on consoles. When playing on consoles, crossplay allows you to join the PC matchmaking for Ranked games and your PC Rank will be different from your console Rank."; "the text chat feature is unavailable on consoles."
- **Crossplay pools**, from the [Collision Point](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/seasons/collisionpoint) season page, December 2024: "PC players will not have the option to join the console matchmaking pools. And any Squad that has a PC player must play in the PC matchmaking pool. When playing Ranked with crossplay active, console players will maintain and progress PC rank that is separate from their console rank".
- **Ranked 2.0's end of squad restrictions, console crossplay and Ubisoft Connect**, from the [Solar Raid](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/seasons/solarraid) season page, December 2022: "Squad MMR restriction is no more, and players can Squad up with any of their friends."; "Now you can play with players from other consoles and Squad up with friends via Ubisoft Connect."; "Each rank now has five divisions … A new rank called Emerald has been added between Platinum and Diamond."
- **Reputation**, from the [Prep Phase](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/seasons/prepphase) season page, March 2025: "DISHONORABLE: Locked from Ranked, Standard, Siege Cup playlists, no access to Ranked rewards"; "DISRUPTIVE: Locked from Siege Cup playlist, no access to Ranked rewards". Standard is gone, so quote neither; state the effect on Ranked and Siege Cup.
- **Siege Cup**, from Ubisoft Help's [Information about the Siege Cup](https://www.ubisoft.com/en-us/help/rainbow-six-siege/purchases-and-rewards/article/information-about-the-siege-cup-in-rainbow-six-siege/000109536) (registration by the squad leader) and the [High Stakes](https://www.ubisoft.com/en-us/game/rainbow-six/siege/news-updates/seasons/highstakes) season page, September 2025: "Siege Cup now runs every weekend". The join and make guides already cite Siege Cup's roster lock; link them, don't repeat the source.

**From the client, through players' reports.** Enough to state as what happens in the game, never cited or quoted:

- **The message:** "HIGH RANK DISPARITY IN SQUAD" on the Ranked tile and "Squad members must be within 15 ranks to play Ranked.", with "Displayed ranks are for PC." in a crossplay squad (screenshot, [r/Rainbow6, 2026-06-06](https://www.reddit.com/r/Rainbow6/comments/1ty4aux/why_cant_we_play_together_it_is_within_15/)).
- **Exactly 15 apart is refused:** Platinum V with Bronze V ([2026-08-17](https://www.reddit.com/r/Rainbow6/comments/1vr23ba/ranked_disparity_says_too_high_but_im_plat_5_and/)); Diamond V with Platinum V, exactly 10 ([2026-10-02](https://www.reddit.com/r/Rainbow6/comments/1ww4nx5/i_dont_understand_the_ranked_30_restrictions/)); Silver III with Emerald II, 16 ([July 2026](https://www.reddit.com/r/Rainbow6/comments/1uo2402/ranked_squad_restriction_blocking_silveremerald/)).
- **Friends who place together land apart:** Bronze and Platinum after five wins together ([2026-09-03](https://www.reddit.com/r/Rainbow6/comments/1w6eqx7/question_about_ranked_30/)).
- **A console player's first PC season places them low:** [2026-10-07](https://www.reddit.com/r/Rainbow6/comments/1wzixbe/not_being_able_to_que_with_lower_or_higher_ranked/).
- **The unlock screen** lists "Upgrade Game Edition / Clearance level 50 / Unlock Attackers 10/10 / Unlock Defenders" ([2026-09-26](https://www.reddit.com/r/Rainbow6/comments/1wqrrbi/completing_these_conditions_can_i_play_rankednever/)).
- **Squads are invited through Ubisoft Connect:** the squad panel reads "Invite friends to play from: Ubisoft Connect", and shows "1/5".
- **r/Rainbow6's automod** sends squad-finding posts to the official Rainbow 6 Discord ([2026-09-02](https://www.reddit.com/r/Rainbow6/comments/1w4vwhq/squad_up_placement_matches/)).

**Not confirmed, or sources disagree.** The guide handles each like this:

- **Where the edge is.** Ubisoft says "within 3 full Ranks", the game "within 15 ranks". Three reports put exactly 15 (and exactly 10 at Diamond) outside; one reply says Silver III could play with Emerald III, exactly 15, and Mobalytics (2026-08-15) says "Diamond 5 can only squad with players from Platinum 5 up through Champion 1". The guide goes with the reports: fewer than 15, fewer than 10. The table's "Always" column holds either way. Before writing, look for a current video or a second report of an exact-15 pair; if it turns out "at most", change the edge sentence and the FAQ, not the table.
- **Whether it's divisions or Rank Points.** One player says the limit is "a 1500 RP difference". With 100 RP a division the two are close but not the same at the edges. The game's message counts ranks; go with divisions, and don't mention RP.
- **A squad of three or more.** No source says the gap is measured between the highest and lowest member, but the message speaks of squad members. State it as following from the message.
- **Party sizes.** No Ubisoft page lists the sizes ranked takes. Squads of up to five queue, as the squad panel shows, and four-stacks are common. Don't claim Ubisoft says so.
- **Y11S3's ranked tweaks**, reported from a developer segment on the Esports World Cup broadcast of 15 August 2026 and in no written Ubisoft note: placement at +100/−50 RP, a 300 RP reset, top Champions reset to Emerald III, wins raised from 25 to 30 RP, and Emerald squads with Platinum or Gold players kept out of Champion lobbies. Leave them all out.
- **The Legend Division's details.** Opening on 15 October 2026 is siege.gg's; Ubisoft says mid-season. Level 200 and console availability are secondary. The guide says only that it's solo queue and above Champion.
- **Rank decay under Ranked 3.0.** Not documented. Siege X added a hidden "skill decay" for returning players, but Ranked 3.0 removed hidden MMR. Leave it out.
- **The console crossplay setting's options.** Only secondary sources name them. Say "turn crossplay on".
- **"Win 5 PvP matches" and "complete the tutorials"** appear in the wiki and the AI Overview, but not in Ubisoft's requirements article. Leave them out.
- **The official Rainbow 6 Discord's invite.** Confirm it on a Ubisoft page before linking; otherwise name it without a link.

## Adding TeamTavern's data

From the development database, a copy of the old site's posts to 2026-09-14 (647 player, 76 group and 24 community posts). Recount from the latest production backup when writing, as the join guide did: its own count on 2026-09-29 was 553 player and 50 group posts, so the copies differ.

- **Ranked is what Siege players want.** "Ranked" is ticked on 85% of player posts and 76% of group posts, against 30% and 41% for scrims and tournaments. One sentence in the introduction or the stack H2.
- **Group ranges are wider than the game allows.** Of 66 group posts with a rank range, 17 span more than three full ranks, 12 of them Copper to Champion. A further 6 span exactly three, which only fits at close divisions. The posts predate Ranked 3.0, and old-site ranks had no divisions, so the importer gave whole ranks; say "about a quarter of group posts ask for a wider range than a ranked squad can now have", from the recount. That's the reason for the advice in the TeamTavern step.
- **Platform.** 62% of player posts play on PC, 28% PlayStation and 11% Xbox. Use it only if the crossplay H2 needs a number.

No chart at launch. The table is the guide's visual. If a chart is wanted later, player posts by rank (Copper 98, Bronze 80, Silver 79, Gold 80, Platinum 107, Diamond 82, Champion 32) shows that every rank has players to recruit from, in the game's order; it's a meaningful rewrite, so change `updated`.

## Internal links

- **Links from this guide:**
  1. `/games/rainbow-six-siege`, twice: in the stack H2 and in the conclusion. Anchor: "Rainbow Six Siege players and groups on TeamTavern".
  2. The make guide, from the stack H2. Anchor: "How to make an esports team".
  3. The join guide, from the introduction ("every game's team format"), and its Siege section and role chart from the stack H2.
- **Links to this guide:**
  1. The join guide: in its "Rainbow Six Siege" H3, after the Siege Cup sentence, one or two sentences: ranked takes a squad of up to five within three full ranks of each other, two with a Diamond or Champion, and a link here with the anchor "How to play ranked with friends in Rainbow Six Siege".
  2. The make guide: a row in its format table, after Deadlock and before Siege Cup: **Rainbow Six Siege: Ranked** | Nobody: a squad of up to 5 queues together, and the game fills empty places | Squadmates within three full ranks, two with a Diamond or Champion, except in placements.
  3. The `/guides` index.
  4. The Siege feed, once feeds link the guides that name their game, as the Marvel Rivals brief proposed.
- Editing the join and make guides adds a fact, so their `updated` changes too.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, and logo-512.png as `image`, as the other guides have. BreadcrumbList Home > Guides > How to play ranked with friends in Rainbow Six Siege. `datePublished` and `dateModified`. No Person.
- Every rule the guide cites links to Ubisoft's own page, on the words that name it, once per source per section. What comes from players' reports (the message, the edge, friends placing apart, the PC rank trap) is stated as what happens in the game, without a link. Reddit and community Discord servers are neither linked nor named; the official Rainbow 6 Discord can be.
- No "when we played" stories.

## Keeping it current

Ubisoft changes ranked at a season's start and sometimes mid-season:

- **Y11S4, around 1–2 December 2026** (the current Battle Pass ends 1 December; Ubisoft gives no date). Read its season page and designer's notes for squad restrictions, placements, crossplay and the social hub. The social hub may give Siege an in-game way to find a squad, which changes the stack H2.
- **Ubisoft's Ranked 3.0 post and help articles**, for the rule and the requirements.
- **The edge**, if Ubisoft ever states it.

Change `updated` only when a fact changes.

## Implementation

- `Server/Guide/rainbow-six-siege-ranked-with-friends.md`, imported as text in `Guides.js`, and an entry in `Guides.purs`: slug, heading, title, description, dates.
- The sentence and link in the join guide's Siege H3, and the row in the make guide's table.
- The cover is already at `/images/games/400/rainbow-six-siege.webp`.
- Add the slug to the guide specs, as the Deadlock guide did. The specs that cover guides must still pass: one table per guide, heading ids for fragment links, and the who-with-whom table at 375 px.

## Timing

1. Write the guide now and launch it as soon as it's checked, with `published` and `updated` set to the real launch day. Nothing in the game waits on it, and launching before Y11S4 lets Google index it before the start-of-season placement and restriction questions.
2. When Y11S4's notes are out, around late November, check the guide against them, update it where a fact changed, change its `updated`, and request indexing again.

## Distribution

The same plan as the other guides:

- request indexing in Search Console on publish
- link it from the join guide, the make guide and the guides index
- r/Rainbow6 sends squad-finding posts away, and its rules on self-promotion apply, so no posts there without checking them
- after 4–6 weeks, check which Siege queries reach the guide and which reach the feed. If a listing query ("lfg", "stack finder") lands on the guide, the guide is competing with the feed, so adjust its title. Also check whether "r6 ranked requirements" queries come, since that H2 may deserve more room.
