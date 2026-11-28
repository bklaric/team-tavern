# Content Brief: Dota 2 party size and playing ranked with friends

The second guide for Dota 2, after the Battle Cup. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the other ranked briefs. It covers what decides whether friends can queue Dota's Ranked together:

- party size: one to five, and how a party queues for Ranked Roles
- what every player in the party needs first: unranked hours, a phone number, and a behaviour score and record that keep them out of low priority
- how far apart a party's MMR can be: the refusal, the warning, and the only number Valve has ever published
- the players who can't party at all: 8,500 MMR and up, and the older Immortal rules
- how a party is matched and what a win or a loss does to each player
- what one player's abandon does to the whole party
- where a group too far apart plays instead: unranked, Turbo and the Battle Cup

Dota 2 is already on the site, so the guide goes out once it's written, and the join guide then links here.

Prepared 2026-11-28, in a cloud session, from [dota-2-ranked-handover.md](dota-2-ranked-handover.md). Evidence comes from:

- Google autocomplete for 26 Dota seeds, fetched from `suggestqueries.google.com` with `hl=en&gl=us`
- the client's own text: `dota_english.txt` from SteamDB's [GameTracking-Dota2](https://github.com/SteamDatabase/GameTracking-Dota2), the copy on `master` fetched 2026-11-28 (52,777 lines)
- every Valve announcement for Dota 2 on Steam from April 2016 to 24 November 2026, through the Steam news API (926 of Valve's own posts and update notes among 2,000 items), searched for party, spread, MMR, Immortal, role queue, phone, hours, behaviour score, abandon and low priority
- every gameplay patch's notes from 7.08 to 7.42a through `dota2.com/datafeed/patchnotes`. They carry no matchmaking rules, so Valve's Steam posts are the only primary source for them.
- the search results for "dota 2 party mmr max difference ranked" and two others, from a search API, and the two 2026 pages among them that give a number, read with curl
- Valve's [Dota2-Gameplay issue tracker](https://github.com/ValveSoftware/Dota2-Gameplay/issues), searched for ranked party reports (players' reports, never linked)
- the Battle Cup brief's facts on calibration, the unranked hours and TeamTavern's Dota posts
- GUIDES.md was not available to this session, so there is no Search Console evidence for Dota here. Check it before the guide is written.

## Why this topic

- **The pattern that works.** Guides that answer how a game's ranked mode treats friends rank at 4–8 within days.
- **Autocomplete asks exactly this, in Dota's own words.** "dota 2 party mmr" gives ten suggestions: "gap", "max difference", "too far apart", "difference", "range", "mmr party limit", "party ranked mmr difference", "solo vs party mmr" and "solo and party mmr". "dota 2 party ranked" adds "ranked party of 4", "party limit", "party queue" and "party size"; "dota 2 rank difference" adds "party", "queue", "max rank difference" and "ranked max mmr difference"; "dota 2 mmr difference party" gives "2025". "dota 2 immortal party" gives "immortal party queue", "immortal rank party" and "immortal draft party". The unlock is the other half: "dota 2 ranked 100 hours", "unlock ranked 100 hours", "how many hours to play ranked", "how to play ranked dota 2 without 100 hours" and "ranked requirements".
- **What ranks is wrong, and wrong in a way the guide can explain.** The pages that answer the party gap give 2,000 MMR (the most repeated figure), "roughly 2,500" (teamsmurf, "Updated July 2026 — Patch 7.41d", saying the Dota 2 wiki gives it) and "approximately 1,000" (teamsmurf again, "Updated August 2026", for every party including five-stacks). The same site gives two numbers a month apart. Valve has published one number: on 10 October 2019 it cut "the acceptable MMR spread within a party from 2800 to 2000", and its notes of 16 October say it reverted that cut "on Friday", 11 October. The 2,000 most pages repeat is the cut that lasted a day. No Valve post since gives a number.
- **The top of the ladder has a hard rule nobody puts beside the gap.** Since 19 March 2025, a player above 8,500 MMR "cannot queue for ranked matches as a member of a party" (Valve). News coverage reported it as an Immortal Draft change, and the ranking guides on party gaps don't connect it to the client's "Party members' MMR is too high to play ranked modes as a party."
- **Dota is a busy feed with only one guide.** It had 25 active posts on 9 October (SUBREDDITS.md), the third-busiest after Valorant and Marvel Rivals, and its only guide is the Battle Cup. 38% of Dota player posts and 53% of group posts tick "Battle Cup" (Battle Cup brief), so a ranked guide speaks to the rest.
- **Dota is where the site's Reddit posts did best** (SUBREDDITS.md), so a guide a Dota player would link has a known audience.

**Why not Dota's pro scene or team MMR.** Valve's own circuit ended with the DPC, and team MMR for a five under a team identity looks like legacy client text (a player's report in Valve's tracker, March 2026, says the official-team option in a five-stack's queue shows nothing in the game). The guide mentions neither.

## Template

**Recommended**: `how-to-guide`, as the other ranked guides. The reader has just been refused, or seen the warning, or wants to know before they invite a friend. What sets the guide apart:

- the client's messages quoted word for word, and what each one means, in one table
- the honest answer on the number: Valve published 2,800 MMR in October 2019, cut it to 2,000 for a day, and hasn't given a number since; players' reports of where the refusal falls today, as reports
- the 8,500 rule, dated and quoted, and the Immortal rules of October 2019 marked as older
- how a party queues for Ranked Roles: both high-demand roles, or tokens, and a five always plays
- the abandon rule that costs the whole party a loss

## Target keywords

- **Primary:** dota 2 party mmr difference, dota 2 party mmr max difference, dota 2 ranked with friends, dota 2 party mmr too far apart
- **Secondary:**
  - dota 2 party mmr gap, party mmr range, mmr party limit, ranked party limit, ranked party difference
  - dota 2 rank difference party, max rank difference, ranked max mmr difference, rank difference queue
  - dota 2 mmr difference party 2025
  - dota 2 ranked party size, ranked party of 4, 4 stack, 5 stack
  - dota 2 immortal party, immortal party queue, immortal rank party, immortal draft party
  - dota 2 solo vs party mmr, solo and party mmr
  - dota 2 ranked requirements, ranked 100 hours, unlock ranked 100 hours, how many hours to play ranked
  - dota 2 5 stack surrender, 5 stack gg
- **Leave to the feed:** "dota 2 party finder", "party finder discord", "dota 2 party discord"
- **Leave out:** rank distribution, medal MMR tables beyond what the gap needs, Immortal Draft's drafting rules, buying or unlocking accounts ("how to play ranked dota 2 without 100 hours" gets the honest answer: you can't, and the FAQ says so)

## Search intent

Mostly a player whose party has just been refused, or who saw the yellow warning and wants to know if it matters: can we queue together, and if not, why? Second, a new player whose friend wants to bring them into Ranked: what do they need first? Third, a high-MMR player who can't queue with anyone and wants to know why. The guide answers each in its first lines and in the FAQ, then sends the reader to find a party within range.

## Recommended title

- **heading (H1):** Dota 2 team size, party size and playing ranked with friends
- **title:** Dota 2 ranked with friends: party MMR gap, party size and Immortal parties
- **slug:** `dota-2-ranked-with-friends`. It matches the sibling ranked guides and still fits if Valve moves the gap or the 8,500 line.

## Meta description

Dota 2 ranked takes parties of one to five, as long as their MMRs aren't too far apart. What Valve has said about the gap, who can't party at all, and what each player needs first.

(The final text gets checked for length against the siblings in `Guides.purs`.)

## TL;DR draft (In short)

- Ranked takes parties of one to five. Every player needs the unranked hours that unlock Ranked, a phone number linked to their Dota account, and must be out of low priority with a behaviour score that allows Ranked.
- A party whose MMRs are too far apart is refused with "Party members' MMRs are too far apart". Valve last gave the limit in October 2019, as 2,800 MMR from the highest to the lowest player, and hasn't given a number since.
- A party inside the limit but still far apart gets a warning instead: it can queue, but Valve says it gets worse matches and longer queues.
- Since March 2025, a player above 8,500 MMR can't queue Ranked as a party at all. Unranked is unchanged.
- A party plays Ranked Roles if it queues both high-demand roles between them, or spends a Role Queue Token each. A full five always can.
- If one player abandons a ranked match before first blood, the whole party takes the loss.

## Information gain

- **The number's history, from Valve's own posts.** 2,800 MMR was the limit until 10 October 2019, when Valve cut it to 2,000 ("We'll consider further reductions to this spread over time, as the current value is still quite large"). It reverted the cut on 11 October and added the warning instead: "we've added a UI indicator when large MMR spreads exist in parties to let players know that being in such parties will negatively impact match quality and queue times." So the 2,000 that ranks is the cut that didn't last, and the warning players see is Valve's own replacement for it. Whether 2,800 still stands, Valve hasn't said; the guide says so.
- **Why medals don't settle it.** Valve: "we always match based on MMR and not medals" (October 2019). A medal shows a range of MMR, so two friends a set number of medals apart may or may not be refused. The guide says to compare MMR, which the client shows each player, not medals.
- **The top of the ladder.** Valve, 19 March 2025: "Players above 8500 MMR who are guaranteed to play an Immortal Draft game may not queue for ranked as a party. Unranked is unchanged." In October 2019 Valve also made Immortals party only "with Immortal or Divine players in Ranked", and counted "every player in that party … the same rank as the highest player". Neither 2019 rule has been restated or withdrawn since; the guide gives them as 2019's rules.
- **Ranked Roles for a party.** The client: "Your party needs to queue for both high-demand roles", or each player "will spend a Role Queue Token"; "Full parties are always eligible for Ranked Roles, but do not earn more Tokens for future use." Valve's notes of 1 July 2026 loosened only the solo rule (one high-demand role), so a party still needs both.
- **One player's problem is the party's.** The client turns the whole party away for one member's low priority ("One or more party members are in the low priority matchmaking pool"), low behaviour score ("A member of your party has a low Behavior Score, preventing you from queueing for Ranked Games"), missing hours or missing phone number. And "When a player abandons a ranked game before first blood, their entire party receives a loss". A player's report in Valve's tracker (September 2026) has a five each losing 30 MMR after one member's Steam crashed during hero selection.
- **Fives.** Since 6 July 2022 a full party can be matched against "any valid opposing team", not only another five, and can surrender after 30 minutes if all five chat "gg", "scored as a loss" (Valve).
- **Party MMR is one number now.** Valve gave solo games 30 MMR and party games 20 from 16 January 2020, on one MMR, and in May 2023 moved gains towards ±25 "for players with high rank confidence", by the teams' ratings. The client's "Party MMR" labels are from the years of separate solo and party MMR. This answers "solo vs party mmr" without a number Valve hasn't restated.

## Content outline

### Introduction (~110 words)

Lead with the answer: Dota's Ranked takes parties of one to five, but refuses a party whose MMRs are too far apart, and Valve's last published limit is 2,800 MMR, from 2019, not the 2,000 most pages repeat. Above 8,500 MMR a player can't party at all. Then "This guide is for …", "It covers …", and the link to every game's team format.

### In short

As above.

### H2: Party size and who can play Ranked

Cover image. Dota is five against five, and Ranked takes parties of one to five ("You must have between 1 and 5 non-coach players to find a match"; coaches aren't allowed in Ranked). What every player in the party needs before the party can queue, each with the client's message:

- the unranked hours: "all party members must have played at least {d:required_play_time} hours of unranked matchmade games". The client's number is a parameter; Liquipedia gives 100, and players and the pages that rank say 100. Hedge as Liquipedia's count, as the Battle Cup guide does.
- a phone number: "Ranked Matchmaking requires a unique phone number associated to your Dota account" (client), since April 2017 (Valve). One number serves one Dota account at a time, with a cooldown before it moves.
- no low priority, and a behaviour score high enough for Ranked. Valve publishes no threshold for Ranked; the client says only that a low score blocks it.

A friend who's new to Dota plays unranked with the party until they unlock Ranked.

### H2: How far apart a party's MMR can be

The messages, quoted: the refusal ("Party members' MMRs are too far apart to participate in ranked matchmaking.") and the warning ("Your party has a larger than recommended difference in ranks between party members. This will result in the matchmaker providing you lower quality matches."). The guide's one table holds these and the other messages a party can see (below).

The number, honestly: 2,800 MMR from highest to lowest until October 2019; cut to 2,000 on 10 October and reverted on 11 October, with the warning added; no number from Valve since. Players' reports of where the refusal falls today go here if any can be found (see Not checked); never as Valve's rule. The pages that give 2,000, 2,500 or 1,000 aren't named; the guide says that pages give smaller numbers and where 2,000 came from.

Why medals mislead (Valve's "MMR and not medals"), and to compare the numbers on each player's profile. Third-party tables put a star at about 150–160 MMR, so 2,800 is roughly three and a half medals; hedge it as an estimate, or leave it out.

Unranked has no such limit: Valve's 2019 post says the spread limit "only applies to ranked matchmaking".

### H2: High MMR and Immortal parties

From 19 March 2025: above 8,500 MMR, "roughly the top 0.5% of players", every ranked game is Immortal Draft and the player can't queue Ranked as a party; they also need an official name. From 6,500 MMR a player plays Immortal Draft only when matched with someone above 8,500. The client's "Party members' MMR is too high to play ranked modes as a party." is read as this rule (hedged: the string isn't dated). The 2019 rules (Immortals party only with Immortal or Divine; an Immortal's party counts at its highest player's rank), given as 2019's, unconfirmed today. The client's "For high MMR Immortals, Role Queue may be disabled." in one line.

### H2: How a party is matched and what it plays for

- Ranked Roles: each player picks the roles they'll play. A party needs both high-demand roles between them, or each player spends a Role Queue Token; a party that covers all five roles earns tokens; a full five always plays Ranked Roles and earns none. Whether a party without tokens can still queue Ranked Classic or All Roles is left to the client check.
- Matching: by MMR, not medals. The client's "Strict Solo Ranked Matchmaking" setting ("Do not match with or against parties") means some solo players opt out of parties (hedged: who has the setting isn't confirmed).
- Fives can meet any valid team since July 2022 and can surrender together after 30 minutes.
- One MMR for solo and party games. Valve's 2020 split (30 solo, 20 party) and its 2023 change, given as dated facts or left out if they can't be put plainly.
- A party that's just inside the limit gets the warning: worse matches and longer queues, by Valve's own words.

### H2: When one player leaves

The client: an abandon before first blood gives "their entire party" a loss, though no other stats are recorded; abandons lead to low priority, and a party with a member in low priority can't queue Ranked. One player's report of a five losing 30 MMR each after a crash in hero selection, as a report. So a party is only as reliable as its least reliable member: queue with people who'll stay.

### H2: Where a party too far apart can play

Two or three sentences: unranked (All Pick, no spread limit, by Valve's 2019 post), Turbo (unranked), and the Battle Cup, which takes a five of any ranks at its highest player's tier, linked to [How the Dota 2 Battle Cup works, and how to find a team for it](/guides/dota-2-battle-cup).

### H2: Finding a party in range

Recruit within the limit with room to spare, since MMR moves every match, and if you're near 8,500, play solo or recruit for unranked and the Battle Cup. Roles: the seed's five positions, Carry (1) to Hard support (5); a party that covers both high-demand roles plays Ranked Roles without tokens, so say which you play (the client names no high-demand role permanently, so the guide doesn't name them). **Post on TeamTavern:** pick your "Rank" (medal and star, "Legend 3"), your "Position", your "Server", and "Ranked" under "Looking for", and add Steam and Discord as contacts. A group post gives a rank range, so keep it narrow enough to queue together. Feed link: `[Dota 2 players and groups on TeamTavern](/games/dota-2)`. Post counts from the production copy go here if they say something (see Adding TeamTavern's data).

### FAQ (6)

- **What is the max MMR difference for a party in Dota 2?** Valve's last published limit is 2,800 MMR, from October 2019. It cut it to 2,000 for a day that month and hasn't given a number since. A smaller gap can still bring the warning.
- **Can Immortal players party in Dota 2 ranked?** Below 8,500 MMR, yes. Above it, not since March 2025; they can still party in unranked.
- **Can you play Dota 2 ranked as a party of four?** Yes; Ranked takes one to five (hedged until the client check, since an old "Parties of four are currently not allowed" string survives).
- **Why can't I play ranked with my friend in Dota 2?** Too far apart, one of you above 8,500, missing hours, a phone number, low priority or a low behaviour score: the table.
- **How many hours do you need to play ranked in Dota 2?** 100 hours of unranked matchmade games, by Liquipedia's count; the client checks it for every party member.
- **Do you lose less MMR in a party in Dota 2?** Hedged per the section above, or answered as: party and solo games move one MMR, and Valve last said in 2020 that party games move it less.

### Conclusion: Find your party (~90 words)

Three or four bullets: compare MMR, not medals, and stay well inside the gap; above 8,500, play solo; every player unlocks Ranked first, with a phone number and a clean record; one player's abandon costs everyone. Then the call to post and the feed link. No official Dota 2 Discord turned up in the Battle Cup research; check once more before writing, and name one only if Valve runs it.

## The guide's one table

The client's messages a party can see, and what to do about each:

| Message | What it means |
|---|---|
| Party members' MMRs are too far apart to participate in ranked matchmaking. | The spread is past the limit |
| Your party has a larger than recommended difference in ranks … | Within the limit, but worse matches and longer queues |
| Party members' MMR is too high to play ranked modes as a party. | Someone is above 8,500 MMR |
| … all party members must have played at least … hours of unranked matchmade games. | Someone hasn't unlocked Ranked |
| One or more party members are in the low priority matchmaking pool. | Someone has low priority games to play |
| A member of your party has a low Behavior Score … | Someone's behaviour score is too low for Ranked |
| All party members must have an associated phone number … | Someone has no phone number linked |

The phone message is the client's general one ("in order to play that game mode"); whether Ranked shows it to a party, or only the per-player "Ranked Matchmaking requires a unique phone number", is for the client check.

## Facts, with sources

Client strings are from `dota_english.txt` on GameTracking-Dota2's `master`, fetched 2026-11-28. They're undated (see Not checked). Steam links follow `store.steampowered.com/news/app/570/view/<gid>`; each answered 200 to curl, but none was read in a browser.

| Fact | Source | Date |
|---|---|---|
| "Party members' MMRs are too far apart to participate in ranked matchmaking." | client, `CompetitiveMMSpreadTooLarge` | current |
| "Your party has a larger than recommended difference in ranks between party members. This will result in the matchmaker providing you lower quality matches." | client, `CompetitiveMMLargeRankSpreadWarning` | current |
| "Party members' MMR is too high to play ranked modes as a party." | client, `CompetitiveMMRankPreventsParties` | current |
| "all party members must have played at least {d:required_play_time} hours of unranked matchmade games"; "{s:mmr_unlock_progress} hours played" | client, `CompetitiveMMNotUnlocked_Party`, `ranked_mmr_lock_progress` | current |
| "Your party is not allowed in ranked matchmaking. One or more party members are in the low priority matchmaking pool." | client, `CompetitiveMMNoLowPriority_Party` | current |
| "A member of your party has a low Behavior Score, preventing you from queueing for Ranked Games."; Behavior Score "dictates availability of features such as … ranked play" | client, `DOTA_MatchmakingRestrictedFromRanked_Description`, `DOTA_PlayerBehaviorSummary_Popup_BehaviorScoreInfo` | current |
| "Ranked Matchmaking requires a unique phone number associated to your Dota account."; a number in use on another account can't move until "a cooldown has passed" | client, `ranked_phone_status_steam_label`, `ranked_phone_anchor_result_error_already_in_use` | current |
| "When a player abandons a ranked game before first blood, their entire party receives a loss, even though stats are not otherwise recorded." | client, `DOTA_Chat_AbandonedRankedBeforeFirstBloodParty` | current |
| "abandoning games in all other game modes may lead to punishment by placement in the low-priority queue" | client, `DOTA_Chat_AbandonLimitedHeroExplanation` | current |
| "You must have between 1 and 5 non-coach players to find a match."; "Coaches are not permitted in ranked matchmaking" | client, `PartyNotValidToMM`, `CompetitiveMMNoCoaches` | current |
| "Your party needs to queue for both high-demand roles."; "Each party member will spend a Role Queue Token and receive their selected roles."; "Full parties are always eligible for Ranked Roles, but do not earn more Tokens for future use."; "For high MMR Immortals, Role Queue may be disabled." | client, `DOTA_Play_RR_PartyMissingQP`, `DOTA_RR_PartySpendingQP`, `DOTA_RR_FiveStack`, `DOTA_RR_Reason` | current |
| "Strict Solo Ranked Matchmaking": "Do not match with or against parties when playing ranked games." | client, `dota_settings_match_solo_queue` | current |
| "Parties of four are currently not allowed in ranked matchmaking."; "MMR is too high to participate in ranked matchmaking in the Reborn beta" | client, `CompetitiveMMNo4Stacks`, `CompetitiveMMRTooHighS2`: old strings that survive; not used | — |
| Phone required for Ranked: "players must register a unique phone number to their account in order to queue for Ranked matches" | [Matchmaking Update](https://store.steampowered.com/news/app/570/view/3819571008275059498) | 2017-04-20 |
| "we're reducing the acceptable MMR spread within a party from 2800 to 2000 … This limitation only applies to ranked matchmaking."; Immortal parties: "every player in that party will be considered the same rank as the highest player in that party"; "we always match based on MMR and not medals" | [Continuing Matchmaking Updates](https://store.steampowered.com/news/app/570/view/3051677487676270896) | 2019-10-10 |
| "On Friday we reverted the change that reduced the hard limit for MMR party spread in ranked matchmaking … we've added a UI indicator when large MMR spreads exist in parties" | [Dota 2 Update – October 16th, 2019](https://store.steampowered.com/news/app/570/view/2404285581858885031) | 2019-10-17 |
| "Immortal players can now only party with Immortal or Divine players in Ranked." | [Dota 2 Update – October 18th, 2019](https://store.steampowered.com/news/app/570/view/2404285581865502481) | 2019-10-18 |
| "makeups like 2-2-1 will valid"; parties of two and three can meet a five "when the need is high" | [December 17th 2019](https://store.steampowered.com/news/app/570/view/3370312875267752000), [November 11th, 2019](https://store.steampowered.com/news/app/570/view/2448197582035255379) | 2019 |
| "solo games will now give 30 MMR and party games will now give 20 MMR"; Ranked Roles and Ranked Classic replace Fast and Slow queue | [Dota 2 Update – January 16th, 2020](https://store.steampowered.com/news/app/570/view/3690071589842569948) | 2020-01-16 |
| "Full parties are now permitted to match against any valid opposing team"; full parties may all chat "gg" to surrender after 30 minutes, "scored as a loss" | [Dota 2 Update – 7/6/2022](https://store.steampowered.com/news/app/570/view/4474904295778019790) | 2022-07-06 |
| Immortal Draft from 6,500 MMR; MMR gains by the teams' ratings, "closer to +/-25 MMR" for high rank confidence | [7.33c Gameplay Patch](https://store.steampowered.com/news/app/570/view/6557848386950497679) | 2023-05-14 |
| Above 8,500 MMR ("roughly the top 0.5% of players"): all ranked games Immortal Draft, an official name required, "may not queue for ranked as a party. Unranked is unchanged."; 6,500+ plays Immortal Draft only with a player above 8,500 | [Dota 2 Update – 3/19/2025](https://store.steampowered.com/news/app/570/view/1794102528280807) | 2025-03-19 |
| "solo players need to select at least one of the high-demand roles in order to not spend a Role Queue Game" | [Dota 2 Update – 7/1/2026](https://store.steampowered.com/news/app/570/view/1836506165566626) | 2026-07-01 |
| Ranked needs "at least 100 hours in unranked matchmaking" | [Liquipedia](https://liquipedia.net/dota2/Weekend_Battle_Cup), via the Battle Cup brief | edited 2026-08-07 |

No Valve post between 19 March 2025 and 24 November 2026 mentions MMR, parties or Immortal Draft beyond the July 2026 role note. 7.42 (18 November 2026) and 7.42a carry no matchmaking changes.

What the ranking pages say (named here, not in the guide):

- 2,000 MMR: the most repeated, in older guides and Steam discussions. It is the cut of 10 October 2019 that Valve reverted on 11 October.
- teamsmurf, "Updated July 2026 — Patch 7.41d": "roughly 2,500 MMR or more", citing "the Dota 2 wiki and long-running community testing", and "Some players report being blocked at gaps closer to 2,000 MMR".
- teamsmurf, "Updated August 2026": "approximately 1,000 MMR", for every party including five-stacks.

Players' reports (hedged, never linked):

- a five each lost 30 MMR when one member's Steam crashed in hero selection, and that member got a 30-minute matchmaking penalty (Valve's tracker, 6 September 2026)
- the official-team option in a five-stack's queue shows nothing in the game (Valve's tracker, 14 March 2026)

## Unconfirmed, kept out or hedged

- **The current spread limit.** Valve's last number is 2,800 (October 2019). Whether it stands, changed silently, or now depends on rank isn't known. The guide says this plainly.
- **Where the warning starts.** Valve gives no number. Left out.
- **The 2019 Immortal rules** (Immortal or Divine only; a party counts at its highest player). Not restated since. Hedged as 2019's rules, or left out if the client check can't show them.
- **That "too high to play ranked modes as a party" is the 8,500 rule.** Inferred from the wording; the string isn't dated.
- **Party sizes.** One to five, from `PartyNotValidToMM`; the four-stack refusal string is assumed unused. Confirm.
- **Ranked Classic.** The client still has "Queue Ranked Classic"; whether a party without tokens can use it today isn't confirmed. Left to the client check.
- **The unranked hours.** Liquipedia's 100; the client's number is a parameter.
- **MMR per game for a party.** 20 by the 2020 note; changed by 7.33c in 2023; players report 30 for a five in 2026. Hedged or left out.
- **The behaviour score threshold for Ranked.** Not published. Left out.
- **Medal-to-MMR conversion.** Third-party estimates only. Hedged or left out.
- **Strict Solo Ranked Matchmaking.** Whether every player has the setting, or only Dota Plus members, isn't confirmed. One hedged line or left out.

## Not checked in this session

This brief was prepared in a cloud session, without GUIDES.md, the production data, the game client or Google's results page. To check on a computer before the guide is written:

**Evidence the workflow asks for**

- **GUIDES.md.** Its Search Console notes for Dota, and whether this topic was already weighed. Neither the repo nor Drive holds it; Drive's copy of the planning folder is from 9 October and has no GUIDES.md.
- **The Google results page and AI Overview** for "dota 2 party mmr max difference" and "dota 2 party mmr too far apart". Chromium got Google's "unusual traffic" page on the first request, so only a search API's results were seen. What the AI Overview says, and which of the 2,000, 2,500 and 1,000 figures it takes, is unchecked.
- **Autocomplete** came from `suggestqueries.google.com` with `hl=en&gl=us`, not from the planning folder's suggest script. "dota 2 ranked with friends", "dota 2 role queue party", "dota 2 can't play ranked with friend" and "dota 2 behavior score ranked" gave nothing.
- **The pages that rank** were seen only as a search API listed them. The two teamsmurf pages were read with curl; the Dot Esports, SteelSeries and Sportskeeda pages, and the Steam discussions from 2019, weren't opened.
- **The Dota 2 wiki's (Fandom's) matchmaking page,** which teamsmurf says gives 2,500. Not opened.
- **TeamTavern's posts.** No backup was available. The counts under "Adding TeamTavern's data" are all still to do.

**Facts to confirm in the game or in a browser**

- **Where the refusal falls.** Queue with friends at known MMRs: about 2,000, 2,500 and 3,000 apart, to see where "too far apart" starts, and where the warning starts. This is the guide's centre, and no current source settles it. Failing that, players' reports from 2025–2026 with MMRs given, read and never cited.
- **That the 8,500 rule shows "too high to play ranked modes as a party".** And whether the 2019 Immortal-or-Divine rule still refuses an Immortal under 8,500 with an Ancient friend.
- **Party sizes.** A party of four in Ranked; the four-stack string should not appear.
- **Ranked Roles for a party.** What a party without tokens is offered: All Roles, Ranked Classic, or neither. Which roles are high-demand on the day, only to describe the screen, not to name in the guide.
- **The phone message a party sees** when one member has no number linked.
- **The unranked hours** the client shows a new account ("hours played"), to confirm 100.
- **MMR per game** in a party and solo, from a recent match each.
- **Dating the client strings.** GameTracking-Dota2's history wasn't searched: GitHub's API for that repository isn't enabled in this session, and a clone of a file this size across its history is too large for the container. `git log -S CompetitiveMMRankPreventsParties` and `-S CompetitiveMMLargeRankSpreadWarning` on a local clone would date the two strings the guide leans on.
- **Valve's Steam announcement pages** answered 200 to curl, but their content was read only from the news API's text. Open each before it's linked.
- **Valve's tracker issues** were read only through GitHub's search results; the repository's own API isn't enabled here.
- **An official Dota 2 Discord.** Not checked again. Name one only if Valve runs it.

**Quotes**

Every Valve quote was checked word for word against the news API's full text, and every client quote against `dota_english.txt`. The teamsmurf quotes were checked against the pages fetched with curl. None was read in a browser.

## Adding TeamTavern's data

The production backup and the cluster's `team_tavern` copy aren't in this cloud session. Before writing, restore the latest backup the user provides and count:

- Dota 2 player and group posts that tick "Ranked"
- group posts whose rank range is wider than roughly 2,800 MMR, by a third-party medal table, if the count says something
- the rank distribution of player posts that tick "Ranked", if it shows where the site's players sit against the 8,500 line (almost certainly nobody: then leave it out)

Use the numbers only if they say something; otherwise leave the section's data line out.

## Internal links

- Opening paragraph: every game's team format, `/guides/join-an-esports-team#your-games-team-format`
- "Where a party too far apart can play": [How the Dota 2 Battle Cup works, and how to find a team for it](/guides/dota-2-battle-cup)
- **Join guide:** the Dota 2 H3 gains the ranked sentence the siblings have, before the Battle Cup link: "Most fives start in ranked, which takes parties of one to five as long as their MMRs aren't too far apart. [Dota 2 team size, party size and playing ranked with friends](/guides/dota-2-ranked-with-friends) covers who can queue together and how to find a party in range." Its `updated` moves.
- **Battle Cup guide:** its "Calibrated for Ranked" bullet can link here for what unlocking Ranked takes. Optional; if it changes, its `updated` doesn't move, since no fact changes.
- **Make guide:** no change. It doesn't link the ranked guides, and its Dota row is the Battle Cup's.

## Trust signals and schema

Organisation as author, Article JSON-LD as the siblings have, `games` naming `dota-2` so the cover shows in the list and on shared links.

## Keeping it current

Recheck when:

- Valve's notes mention parties, MMR spread, Immortal Draft or Ranked Roles again (search the news API as above)
- the client's `CompetitiveMM*`, `DOTA_RR_*` or `DOTA_Play_RR_*` strings change (grep GameTracking-Dota2)
- a new ranked season or MMR reset is announced

## Implementation

As the skill sets out: the guide in `Server/Guide/dota-2-ranked-with-friends.md`, registered in `Guides.js` and `Guides.purs`, the join guide's Dota 2 H3, a test in `guides.spec.ts`, and the slug in `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and `screenshots.mjs`. The guide's one table is the client's party messages and what each means.
