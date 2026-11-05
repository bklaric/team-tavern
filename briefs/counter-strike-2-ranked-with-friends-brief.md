# Content Brief: Counter-Strike 2 party size and playing Premier with friends

The fifteenth game guide, and the first for Counter-Strike 2. It is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like the Valorant, Overwatch and League of Legends ranked briefs. It covers what decides whether friends can queue CS2's ranked modes together:

- party size in Premier, Competitive and Wingman
- what a player needs before Premier opens: Prime and Profile Rank 10
- how far apart CS Ratings can be in Premier, by party size, and why nobody can give an exact number
- friends without an established CS Rating
- Competitive's per-map Skill Groups and a friend without Prime
- FACEIT's party limits, for players who move on
- the ban penalty that reaches everyone a cheater partied with

Counter-Strike 2 is already on the site, so the guide goes out once it's written, and the join guide then links here.

Prepared 2026-11-05. Evidence comes from:

- GUIDES.md's Search Console notes (CS2's feed draws only LFG queries; no ranked query reaches the site)
- Google autocomplete for 19 CS2 seeds (`%LOCALAPPDATA%/Temp/gsc/suggest-cs2.mjs`)
- the Google results and AI Overview for "cs2 premier rating difference to play with friends", loaded on 2026-11-05 with `hl=en&gl=us`
- the CS2 client as of the latest build in SteamDB's [GameTracking-CS2](https://github.com/SteamDatabase/GameTracking-CS2): `csgo_english.txt` (every matchmaking error and tooltip), `mainmenu_play.js` (what unlocks Premier) and the repo's history (the commit that added the five-stack message); copies in `%LOCALAPPDATA%/Temp/cs2-strings/`
- every Counter-Strike announcement on Steam since 2012, through the Steam news API (`news.json`, searched by `news.mjs` and `show.mjs` in the same folder)
- FACEIT's [CS2 Match Types FAQ](https://support.faceit.com/hc/en-us/articles/16873836685340-CS2-Match-Types-FAQ-Super-Match-and-Premium-Match) (updated 19 Nov 2025), read in the browser because it refuses WebFetch
- Steam Community discussions (Feb 2024 to Feb 2025) and r/GlobalOffensive and r/cs2 threads (Oct 2025, Mar 2026), read only, never cited

## Why this topic

- **The pattern that works.** Guides that answer how a game's ranked mode treats friends rank at 4–8 within days.
- **Autocomplete is full, and it asks this exact question.** These seeds each give ten suggestions: "cs2 premier with friends" (among them "cant queue with friends", "with unranked friend", "gap to play with friends"), "cs2 premier party" ("rank difference", "restrictions", "limit", "range"), "cs2 rank difference" ("to play together", "limit 2025", "limit premier"), "can you play premier with" ("without prime", "with 4 people", "with unranked", "with a comp ban") and "cs ratings of players in your party" ("are too high both unranked", "too far apart", "too high season 3"). "Cs2 premier rating difference" gives nine, "cs2 premier duo" six ("rank difference", "limit", "restrictions").
- **Nobody has the answer right, so being right is the edge.** Valve has never published the number. The pages that rank give 5,000 (the most repeated figure, from a Steam post of July 2024), "5,000 to 10,000", or "5,000 for duos, 10,000 for five-stacks". The AI Overview says five-stacks have "no rating difference limit" since late 2025 and duos "roughly 5,000 to 10,000". Players who've hit the limit since report roughly 8,500 to 9,500 for parties under five. Valve's client still shows a plain "too far apart" message beside the five-stack one, so a limit for five may remain.
- **What Valve does say is in its own client and patch notes,** and none of the ranking pages quotes it: three error messages, the Premier unlock in the play menu's code, and four patch notes that moved the limit.
- **CS2 is one of the site's feeds without a guide.** Its feed draws only LFG queries.

**Why not more on FACEIT.** FACEIT is a third party with its own ladder. It gets one short section with its published party limits, because a Premier group outgrowing Premier is a common next step, and because its rules answer the same question cleanly.

## Template

**Recommended**: `how-to-guide`. The reader has just been refused, or wants to know before they try, whether they can queue with a particular friend, and what to do instead. What sets the guide apart:

- Valve's three messages quoted word for word, and what each one means
- the honest answer on the number: unpublished, changed four times, and what players hitting it report since the last change
- why a friend without a visible rating can still be refused
- the Premier unlock from the client's own code, Prime and Profile Rank 10
- the five-stack exception dated to the build that added it, 3 October 2025, which its patch notes don't mention

## Target keywords

- **Primary:** cs2 premier with friends, cs2 premier rating difference, cs2 premier party restrictions, cs2 rank difference to play together
- **Secondary:**
  - cs2 premier rating difference limit, max rating difference, rating difference to queue, gap to play with friends
  - cs2 premier duo rank difference, duo limit, duo restrictions
  - cs2 premier party limit, party range, party rules
  - cs ratings of players in your party are too far apart, are too high, too high both unranked
  - cs2 premier with unranked friend, can you play premier with unranked
  - can you play premier without prime, cs2 premier requirements
  - can you play premier with 4 people, cs2 premier 4 stack
  - cs2 can't queue with friends, why can't i play premier cs2 with friends
  - cs2 competitive with friends, cs2 wingman with friends
  - cs2 party member banned

## Search intent

Mostly a player who has just seen one of the error messages, or whose friend's rating is far from theirs, and wants to know whether it's fixable. Second, a player planning to queue with a new friend: will we be allowed? Third, a group of five that wants to know whether a five-stack gets round the limit. The guide answers each in its first lines and in the FAQ, then sends the reader to find players in range.

## Recommended title

- **heading (H1):** Counter-Strike 2 team size, party size and playing Premier with friends
- **title:** CS2 Premier with friends: rating difference, party size and unranked friends
- **slug:** `counter-strike-2-ranked-with-friends`. It matches the sibling ranked guides and still fits if Valve reworks Premier or Competitive.

## Meta description

Counter-Strike 2 team size is five. Premier with friends: how far apart CS Ratings can be, what a five-stack allows, why an unranked friend can't join, and Prime.

(The final text gets checked for length against the siblings in `Guides.purs`.)

## TL;DR draft (In short)

- Premier and Competitive are five against five, and take parties of one to five. Fours have been allowed in Premier since November 2023.
- Premier needs Prime and Profile Rank 10 for every player in the party. Competitive and Wingman unlock at Profile Rank 2.
- Valve doesn't publish how far apart CS Ratings can be. Players report that a party of two to four can be up to about 9,000 apart. Since October 2025, a full five-stack can queue much further apart.
- A player whose rating is very high can't party with someone who hasn't yet won the 10 Premier matches that set a rating. Even friends without a visible rating can be refused, because the game already keeps a hidden one.
- In Competitive, a party too far apart in Skill Groups can only queue as a full five. A friend without Prime makes the match Unranked.

## Information gain

- The client's three Premier messages, quoted, and what each means: "too far apart" (no party size will do), "too far apart … unless you play with a full team of five players" (find a fifth) and "too high … with a player who does not have an established CS Rating".
- The four changes to the limit: "Adjusted the range of CS Ratings allowed to party together" (6 Feb 2024), "Adjusted the logic that determines when players are too far apart" (6 Feb 2025), the five-stack message added in the build of 3 Oct 2025 with no patch note, and fours allowed from 8 Nov 2023. So any number dated before October 2025 is out of date for five-stacks, and before February 2025 for everyone.
- Why the edge is fuzzy: the check appears to run on the hidden rating, so friends who see 16,322 and 7,858 can be refused while a pair further apart on screen isn't.
- The Premier unlock from `mainmenu_play.js`: Prime, and Profile Rank 10 or a prestiged profile, and no Premier off-season.
- A non-Prime friend in a Competitive party: "Lobby member is not enrolled in Prime. Prime requires all lobby members to be enrolled. This match will use skill-based matchmaking but will not affect players' skill groups or leaderboards."
- The ban penalty on everyone a cheater partied with, from Valve's launch notes.

## Content outline

### Introduction (~110 words)

Lead with the answer: Valve doesn't publish the limit. Parties under five are refused somewhere around 9,000 apart, by players' reports, not the 5,000 most pages give, and a five-stack has queued much wider since October 2025. Then "This guide is for …", "It covers …", and the link to every game's team format.

### In short

As above.

### H2: Team size and party size

Cover image. Premier and Competitive are 5v5. Parties of one to five, and Premier took fours from 8 Nov 2023, when Valve also gave a solo player matched with a party of four immunity from vote-kicks. Wingman is 2v2, so a party of two. What every player in the party needs: Premier, Prime and Profile Rank 10 ("Sergeant Rank 10"), and it closes between seasons ("Premier season has ended. Please wait until the next Premier season begins."). Competitive and Wingman open at Profile Rank 2 ("This game mode unlocks when you reach Private Rank 2."). A friend who's just installed the game plays Casual or Deathmatch first.

### H2: How far apart CS Ratings can be in Premier

The three messages, quoted, and what each one means. The honest answer on the number: Valve hasn't published it, and has changed it in four updates (dates above). Players' reports since the last change: parties under five refused at around 8,500 to 9,500 apart, and sometimes allowed, sometimes not, near the edge. Five-stacks allowed at 8,700 with 21,000 and 16,400 with 30,500 in October 2025. All hedged as players' reports, never as Valve's rule. Why the edge moves: the check appears to use the hidden rating, which isn't the number on screen. A party that's refused once can be allowed after a few matches, and the other way round.

### H2: Friends without a CS Rating

A rating appears after 10 Premier wins, and each new season everyone wins 10 again (Valve, Season Two; hedged as Season Two's rule if no later post repeats it). Valve's launch note: "players with a very high established CS Rating are not allowed to party with accounts that do not have an established CS Rating". Players report being refused when neither has a visible rating yet, because the hidden one is already far apart. A rating that's expired from inactivity comes back after one win ("Win 1 match to display it again"). What to do: play Competitive together until the friend has a rating, or a five-stack.

### H2: Competitive and Wingman

Competitive keeps a Skill Group per map, eighteen of them, Silver I to The Global Elite. Valve expired every per-map Skill Group on 21 Jan 2026 and re-shows each after enough wins. The message: "Competitive Skill Groups of players in your party are too far apart to participate in competitive matchmaking unless you play with a full team of five players", so in Competitive a full five can always queue. The other one: a party whose Skill Groups are high can't queue with a player who has none. Valve gives no distance. Prime: a non-Prime friend makes the match Unranked (quoted). Unranked has "no restrictions on players in your party". Wingman has its own Skill Group. Leave out how its gap works, since nothing says.

### H2: FACEIT

FACEIT's [CS2 Match Types FAQ](https://support.faceit.com/hc/en-us/articles/16873836685340-CS2-Match-Types-FAQ-Super-Match-and-Premium-Match): Standard matches have "no restrictions on party size or party Elo". Super Match takes parties of one to three, "Premade parties cannot exceed a 400 Elo difference", and a party with a player above 2,800 Elo can go wider with Elo penalties. Premium Match takes one or two, for subscribers who are Verified and Veteran, in EU and NA. So on FACEIT, a five of any levels plays Standard.

### H2: Who you queue with matters

Valve's launch notes: "When one or more party members get convicted of cheating and permanently banned, all their associates will be penalized with loss of Profile Rank and CS Rating". The client says it to the player: "One or more members of your recent parties were convicted of cheating … All their associates, including you, have been penalized." Queue with people you know, or keep the ones you've played with. This answers "cs2 party member banned".

### H2: Finding players in range

Recruit within about 9,000 of everyone, with room to spare since ratings move every match. A party that can't fit plays as five, or in Competitive. Roles: the seed's six. Say which you're short of. **Post on TeamTavern:** the "Premier rating" field (in steps of a thousand, "12k"), "Competitive skill group", "Wingman rank" and "Faceit level", "Ranked" under "Looking for", and Steam and Discord as contacts. A group post gives a Premier rating range a party under five could actually queue across. Feed link: `[Counter-Strike 2 players and groups on TeamTavern](/games/counter-strike-2)`. Post counts from the production copy go here if they say something (see Adding TeamTavern's data).

### FAQ (6)

- **How far apart can you be in CS2 Premier?** Valve doesn't say. Players report about 9,000 for a party of two to four, and more for a full five since October 2025.
- **Can you play Premier with 4 people?** Yes, since 8 November 2023.
- **Can you play Premier without Prime?** No. Premier needs Prime and Profile Rank 10. A non-Prime friend can play Competitive with you, as an Unranked match.
- **Why can't I play Premier with my unranked friend?** The two messages and the hidden rating.
- **Can a five-stack be any rating in CS2?** In Competitive, yes. In Premier, wider than a smaller party, and players have queued 14,000 apart, but Valve still shows a message for parties too far apart even as five.
- **What does "CS Ratings of players in your party are too high" mean?** Someone in the party has a high rating and someone else has no established rating yet.

### Conclusion: Find your stack (~90 words)

Three or four bullets: around 9,000 for under five, a full five for wider; Prime and Profile Rank 10 first; an unranked friend plays Competitive until rated; queue with people you trust. Then the call to post and the feed link. No official CS2 Discord server turned up in this research; check before writing, and name one only if Valve runs it.

## Facts, with sources

| Fact | Source | Date |
|---|---|---|
| "CS Ratings of players in your party are too far apart to participate in matchmaking together." | client, `SFUI_QMM_ERROR_PremierRatingLargeDelta` | build of 2026-11-03 |
| "… unless you play with a full team of five players." | client, `SFUI_QMM_ERROR_PremierRatingLargeDelta5`; added in GameTracking-CS2 commit `2e50617` (build 2000650) | 2025-10-03 |
| "CS Ratings of players in your party are too high to participate in matchmaking with a player who does not have an established CS Rating." | client, `SFUI_QMM_ERROR_PremierRatingMissing` | current |
| Competitive Skill Groups "too far apart … unless you play with a full team of five players"; "too high … with a player who does not have a Skill Group" | client, `SFUI_QMM_ERROR_SkillGroupLargeDelta`, `SkillGroupMissing` | current |
| Unranked: "No restrictions on players in your party" | client, `play_maps_section_tooltip_unranked` | current |
| Non-Prime lobby member: "Prime requires all lobby members to be enrolled. This match will use skill-based matchmaking but will not affect players' skill groups or leaderboards." | client, `tooltip_prime-lobby_has_nonprime_player` | current |
| Premier available only with Prime (`GetElevatedState() === 'elevated'`) and Profile Rank 10 or prestige, and not in off-season; other modes need level 2 | client, `mainmenu_play.js`, `_IsGameModeAvailable` | current |
| "This game mode unlocks when you reach Private Rank 2"; per-player errors name "Sergeant Rank 10" | client strings | current |
| "Purchase Prime Account Status, and complete matches in other game modes to unlock this one." | client, `PlayMenu_unavailable_newuser_2_nonprime` | current |
| "Premier season has ended. Please wait until the next Premier season begins." | client, `SFUI_QMM_ERROR_PremierSeasonOver` | current |
| "Parties of four are now able to queue for Premier mode"; "vote-kick immunity for solo players who get matched on the same team with a party of four players" | [Release Notes for 11/8/2023](https://store.steampowered.com/news/app/730/view/5302486387996463019) | 2023-11-08 |
| "In Premier, players with a very high established CS Rating are not allowed to party with accounts that do not have an established CS Rating"; associates of a banned cheater "penalized with loss of Profile Rank and CS Rating" | [Release Notes for 9/27/2023](https://store.steampowered.com/news/app/730/view/5220291886484673985) | 2023-09-27 |
| "Adjusted the range of CS Ratings allowed to party together in Premier matchmaking" | [Release Notes for 2/6/2024](https://store.steampowered.com/news/app/730/view/5589599109259623924) | 2024-02-06 |
| "Adjusted the logic that determines when players are too far apart in CS Rating …"; "In most cases, players who used to play together as a party in Premier Season One will be able to party together in Premier Season Two" | [Counter-Strike 2 Update, 6 Feb 2025](https://store.steampowered.com/news/app/730/view/1790848102633267) | 2025-02-06 |
| "re-establish their CS Rating by winning 10 matches" at a new season | [Counter-Strike 2 Update, 28 Jan 2025](https://store.steampowered.com/news/app/730/view/1790214123031344) | 2025-01-28 |
| Rating and Skill Group expire with inactivity; "Win 1 match to display it again" | client, `tooltip_cs_rating_expired`, `tooltip_skill_group_expired` | current |
| Per-map Skill Groups expired, "displayed again once the player obtains the required number of wins" | [Counter-Strike 2 Update, 21 Jan 2026](https://store.steampowered.com/news/app/730/view/1822556746157780) | 2026-01-21 |
| Season Five began | [Counter-Strike 2 Update, 8 Jul 2026](https://store.steampowered.com/news/app/730/view/1836506165581827) | 2026-07-08 |
| FACEIT Standard: "no restrictions on party size or party Elo"; Super 1–3 within 400 Elo; Premium 1–2, subscribers, Verified, Veteran, EU and NA; 2,800+ may exceed with penalties | [FACEIT CS2 Match Types FAQ](https://support.faceit.com/hc/en-us/articles/16873836685340-CS2-Match-Types-FAQ-Super-Match-and-Premium-Match) | updated 2025-11-19 |

The store links follow the pattern `store.steampowered.com/news/app/730/view/<gid>` from each item's gid in the news API. Each one gets opened before it goes in the guide.

Players' reports (hedged, never linked):

- Before Oct 2025: a limit of around 9,500 for every party size (Oct 2025 thread). A duo of 3.7k and 12k allowed (Dec 2024). Friends with no visible rating refused, with 9, 8 and 3 wins (May 2024) and in a five (Jan 2025).
- After 3 Oct 2025: five-stacks of 8,700 to 21,000 allowed (7 Oct 2025), and 16,400 to 30,500 seen as opponents (7 Oct 2025). A duo of 16,322 and 7,858, 8,464 apart, refused (1 Mar 2026). 13k with 22k "sometimes we have issues, sometimes not" (Mar 2026). Estimates of "somewhere between 8.5 and 9.5".

## Unconfirmed, kept out or hedged

- **The numbers.** Valve publishes none. The guide gives players' reports as reports, with a range, never a single figure as a rule.
- **Whether five-stacks have any limit.** The five-stack message implies a wider limit; the plain message still exists and may still fire for five. Hedged.
- **What rating the check uses.** The hidden rating, players infer, from refusals that don't match the numbers on screen. Hedged.
- **Whether every season still needs 10 wins.** Stated by Valve for Season Two only. Later season posts don't repeat it. Hedged as how Season Two began, or confirmed in the client's `cs_rating_wins_needed` text if the number shows.
- **Which map's Skill Group counts in a Competitive party check,** and how far apart counts as "too far". Not stated anywhere. Left out.
- **Wingman's party check.** Left out.
- **When Season Five ends.** No date published. Not stated.
- **Premier Private Queue.** The code plays it as a Premier match without the Prime preference. Whether it applies party limits or changes ratings is unknown, so it's left out.
- **CS Rating colour bands.** Not needed; left out.

## Adding TeamTavern's data

Docker wasn't running when this brief was written. Before writing, restore the latest production backup the user provides, or use the cluster's `team_tavern` copy, and count:

- CS2 player and group posts that tick "Ranked"
- if new posts carry a Premier rating range: how many group ranges are wider than about 9,000, which a party under five couldn't queue across

Imported posts from before the 2026 relaunch didn't carry CS2's rank answers over whole (the make guide's chart leaves CS2 out of the rank row), so the range count may be too thin to use. Then the guide leaves it out.

## Internal links

- Opening paragraph: every game's team format, `/guides/join-an-esports-team#your-games-team-format`
- Recruiting: the make guide, and the join guide's CS2 section for ESEA (`/guides/join-an-esports-team#counter-strike-2`)
- **Join guide:** the CS2 H3 gains the ranked sentence the siblings have: "Most players start in Premier, where a party of up to five queues within a CS Rating range Valve doesn't publish, and a full five can queue wider. [Counter-Strike 2 team size, party size and playing Premier with friends](/guides/counter-strike-2-ranked-with-friends) covers who can queue together and how to find players in range." Its `updated` moves.
- **Make guide:** no change. It doesn't link the ranked guides, and its CS2 row is ESEA's.

## Trust signals and schema

Organisation as author, Article JSON-LD as the siblings have, `games` naming `counter-strike-2` so the cover shows in the list and on shared links.

## Keeping it current

Recheck when:

- Valve's notes mention parties, CS Rating ranges or Skill Groups again (search the news API for "party")
- a new Premier season begins (Season Six, date not announced)
- the client's `SFUI_QMM_ERROR_Premier*` or `SkillGroup*` strings change (grep the GameTracking-CS2 history)
- FACEIT's Match Types FAQ moves its "Updated" date

## Implementation

As the skill sets out: the guide in `Server/Guide/counter-strike-2-ranked-with-friends.md`, registered in `Guides.js` and `Guides.purs`, the join guide's CS2 H3, a test in `guides.spec.ts`, and the slug in `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and `screenshots.mjs`. The guide's one table is the three Premier messages and what to do about each.
