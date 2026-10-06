# Content Brief: Playing ranked with friends in Deadlock, and finding a duo

The fourth game guide, and a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md), like [the-finals-ranked-with-friends-brief.md](the-finals-ranked-with-friends-brief.md), which it follows closely. It takes Deadlock's Ranked Mode and covers what a player needs to know to play it with friends: who can queue together, what a friend has to do before they can, where a group of three to six plays instead, how a duo shares a lane, and how to find a duo partner within reach. It goes out with Deadlock on the site, while the game is still an invite-only playtest, so the page is indexed before Valve opens it. The join and make guides then get a Deadlock entry that links here.

Prepared 2026-10-06. **Deadlock's ranked rules may change when the next ranked period starts, on or after 2026-10-08, so this brief is checked against that before the guide is written (see Timing).** Evidence comes from:

- the current Google results for the target queries, loaded on 2026-10-06, and Google's autocomplete for 22 Deadlock queries (`suggest.cjs`)
- Valve's own text: the update posts on Steam news, the changelog threads on forums.playdeadlock.com, the Steam store page, and the current client's strings and `ranked_seasons.vdata` as mirrored by the SteamTracking GameTracking-Deadlock repository (build 6753, 2026-10-05)
- r/DeadlockTheGame threads from July to October 2026
- 704 distinct messages from the LFG channels of the Deadlock Community, Statlocker and Night Shift Discord servers, read for the seed. Most of them are from September and October 2026, 459 from October. They are in `C:\Users\BranimirKlaric\deadlock-research\`; `ranked-asks.cjs` counts what they say about ranked, duos and stacks, and `intents.cjs` counts what they look for
- deadlock-api.com's [Solo vs duo queue win rates in Deadlock ranked](https://deadlock-api.com/blog/solo-vs-duo-queue-ranked) (Manuel Raimann, 2026-09-07), read in full on 2026-10-06, for the guide's chart

TeamTavern has no Deadlock posts yet, so this guide has no data of its own at launch. See "Adding TeamTavern's data" below.

## Why this topic

Chosen on 2026-10-06 over two others:

- **Getting an invite.** "How to get a Deadlock invite" is the biggest search around the game, but every Deadlock site and a Steam forum megathread already answer it, it ends the day Valve opens the game, and it would bring people to a feed that has no posts yet. It gets one FAQ answer here instead, easy to delete at launch.
- **Community tournaments.** Valve runs nothing; everything is community-run, and the open part is small (see "From a stack to a team"). A short H2 here, not a guide.

Ranked with friends has the questions behind it, rules Valve set on 2026-07-30 that most pages carry only in part, and a partner problem that leads to TeamTavern.

## Template

**Recommended**: `how-to-guide`. The reader wants to know whether they can queue with their friends, what's stopping them, and where to play if they can't. What sets the guide apart is the full set of party rules, each sourced to Valve and turned into advice on who to play with and where.

## Target keywords

The site has never had Deadlock, so Search Console has nothing. `/games/deadlock` should take every listing query. This guide targets what a listing can't answer:

- **Primary:** deadlock ranked with friends, and can you play deadlock ranked with friends
- **Secondary:**
  - deadlock ranked party size, and deadlock ranked party limit
  - deadlock duo queue, and deadlock duo ranked
  - deadlock playing with low rank friends
  - deadlock can't play ranked with friends, and deadlock cannot determine party member eligibility
  - deadlock 6 stack queue, and deadlock 6 stack queue times
  - deadlock lane with friends, and deadlock lane with friends setting
  - deadlock queue with friends
- **Leave to the feed:** "deadlock lfg", "lfg discord", "duo finder", "team finder"
- **Leave out:**
  - hero tier lists, duo combos, builds and climbing tips. That's gameplay, owned by stats and booster sites, and it changes with every patch.
  - rank distribution, rank points per game and win streak bonuses. They're about climbing, not people, and Valve's own numbers disagree (see "Not confirmed").
  - Street Brawl. It doesn't touch rank, and nobody has published its party size.
- **Questions**, from Google's People Also Ask on 2026-10-06 and the autocomplete:
  - Can you play Deadlock ranked with friends?
  - Why can't I play ranked with my unranked friend?
  - Can you see friends' rank in Deadlock?
  - Can three friends play Deadlock ranked?
  - How do I invite a friend to the Deadlock playtest? ("deadlock how to invite friends", "how to get deadlock invite from friend")

Measure after launch instead of guessing volumes: see Distribution.

## Search intent

The intent is informational. The reader has a friend or a group to play with, has hit a ranked restriction or heard about one, and wants to know what the game allows and where to play instead.

**Google's AI Overview gets the rules right this time**, unlike The Finals'. For "deadlock ranked with friends" it gives solo or duo only, one rank apart, Eternus within three subranks and solo calibration, citing Boosting Factory. For "deadlock cannot determine party member eligibility" it lists the 60 wins, the three heroes, calibration, the rank spread and sanctions. So the guide's edge is not correcting an answer but going past it: who each rank can duo with, what to do with friends who aren't eligible yet, where a stack plays and what it costs, the lane setting, and finding a partner.

The results fall into four kinds:

- **Out-of-date answers.** Inven Global's "Can You Play Deadlock Ranked as a Party with Friends" (2024-10-23) is the first result for both primary queries, under four duplicate URLs, and says "No, you can't", describing the solo-only ranked of October 2024. Steam threads from 2024 and pages listing the old ranks (Alchemist, Arcanist, Archon) also rank.
- **Party-rule pages.** dramalock.org's "Deadlock Party Size: Ranked, Duo And 6-Stack Rules" (2026-07-31, updated 2026-08-09, ~2,100 words) is correct and covers Standard and the 6-stack. It has nothing on lanes, rank pairs, queue times or finding a partner.
- **Booster sites.** rankforge.gg's duo queue guide (2026-09-20, ~5,500 words, 30 H2s) is the strongest competitor: a rank-pair table, calibration, lanes in general terms, the 15-win hero rule, with boosting upsells throughout. It doesn't mention the lane preference setting, queue times or where to find a partner. Boosting Factory, Skycoach and neonsect.com carry the rules inside general ranked guides; esports-watch.com leaves out the Eternus rule.
- **Complaints.** "To all the people complaining about not being able to play ranked with friends" (r/DeadlockTheGame, about August 2026, 140+ comments) and a Steam thread "Please Reconsider the Ranked Duo Restriction" (2026-07-30) rank for the primary query.

Say in the first paragraph who the guide is for: a player who wants to play ranked with a friend, or has a group that can't, and wants to know who they can queue with and how to find a partner.

**Players run into the rules in four ways**, all in r/DeadlockTheGame:

- **Trios are shut out.** "I mainly play with two friends… the game forbids trios in ranked queue" ([2026-07-30](https://www.reddit.com/r/DeadlockTheGame/comments/1vb59b5/)); "I'm getting excluded because I'm the weakest link and have the fewest hours" ([2026-08-04](https://www.reddit.com/r/DeadlockTheGame/comments/1vf6qzm/)).
- **Calibration splits a duo.** One friend placed two ranks above the other and they could no longer queue: "I could lose on purpose" ([2026-08-02](https://www.reddit.com/r/DeadlockTheGame/comments/1vdqluf/)); "it's been a whole lot quicker for him to just start bagging down" ([2026-08-07](https://www.reddit.com/r/DeadlockTheGame/comments/1viccze/)).
- **Big stacks wait.** "6 man stack – 30 minute queue" ([2026-08-01](https://www.reddit.com/r/DeadlockTheGame/comments/1vcbkwr/)); "more than 3 … almost always 8+ minutes" ([2026-08-12](https://www.reddit.com/r/DeadlockTheGame/comments/1vmekk9/)). Another player reports 3–5 minutes for a 6-stack in NA and EU.
- **No duo to play with.** "I have no friends and it is impossible to find a ranked duo" ([2026-09-06](https://www.reddit.com/r/DeadlockTheGame/comments/1w8kl07/), a Phantom player). The replies point to Discord servers and to asking a good lane partner to keep queueing.

**What the LFG channels show.** Of the 704 messages, 114 look for a duo, 37 mention ranked and 20 both. Ranked duo posts give a rank, and often two neighbouring ranks, which is the one-rank window: "lfd ranked Acolyte - Sentinel (no childish behavior)" (2026-10-04), "lf sentinel/mystic ranked duo" (2026-10-06), "lf duo oce seeker/aco ranked games" (2026-09-30). Some look for a duo or a stack in Standard to unlock ranked: "playing casual to unlock heroes for ranked" (2026-10-04), "lf DUO TO TRY N WIN SOME ALT CHAMPS UP FOR 15/15 RANKED" (2026-10-06). Only 20 look for a stack of three or more. One wants a duo "to lane w" (2026-10-06).

The guide doesn't link or name Reddit or community Discord servers; the user decided that on 2026-10-06. Sources are primary: Valve for the rules, the Death Slam League for its own rules (the way the make guide links ETF2L and RGL), and deadlock-api.com for its own data.

## Content parameters

- **Word count:** 1,700–2,100. The join guide's Deadlock section stays at 3–4 sentences and links here.
- **Reading level:** plain and direct, like the other guides. It says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the other guides, Markdown in `Server/Guide/`.
- **H2 sections:** 6, plus an FAQ.
- **Visuals:**
  - the game's cover, as in the other guides
  - one table: each rank and who it can duo with. The specs' `getByRole("table")` is strict, so there's only one table. Two columns (Your rank / Can duo with), twelve rows with Obscurus, checked at 375 px against `phone.spec.ts`.
  - one chart, in the duo partner H2: how much a duo gains over playing alone, from deadlock-api.com (see that H2)
- **FAQ:** 5 questions.
- **Naming:**
  - "Deadlock", "Valve".
  - "Ranked Mode" once, then "ranked". "Standard Mode" once, then "Standard", as the client has it; say once that it's the unranked mode.
  - Ranks as Valve writes them, with Roman subranks: "Mystic IV", "Eternus I", as the seed has them. Not the metal nicknames some players use.
  - A "duo" is a party of two and a "stack" a party of three to six, as players say; "a 6-stack" for a full team. "Calibration" for the first eight ranked games, as Valve says, not "placements".
  - "Lane preference" and its choices "Balanced" and "With Party", as the client writes them.
  - Name no season in the body. The guide's `updated` date carries when it was true. Where a rule's age matters, say "since July 2026".

## Recommended title

**Deadlock ranked with friends: who can duo, and where a stack plays**

Alternatives:

1. Can you play Deadlock ranked with friends? Duo rules, calibration and 6-stacks
2. How to play Deadlock ranked with a friend, and find a duo within one rank

H1: "How to play ranked with friends in Deadlock".

Slug: `deadlock-ranked-with-friends`. It's permanent, so it names the subject, not "duo finder" and not a season.

## Meta description

Deadlock ranked takes solos and duos one rank apart, after eight solo calibration games. Who can queue together, where a stack plays, and how to find a duo.

## TL;DR draft

> - Ranked takes solo players and duos only. A group of three to six plays Standard, which has no limits on party size or rank.
> - A duo must be at most one rank apart, so a Mystic can duo with a Sentinel or a Ritualist. Eternus players duo only with another Eternus within three subranks.
> - Before a friend can join you in ranked, they need 60 wins in Standard, three heroes with 15 wins each, and eight calibration games played alone.
> - Calibration places each player from their own history, so friends can come out more than a rank apart and lose their duo.
> - A party can ask the game to put its members in the same lane: set Lane preference to With Party.
> - A duo wins only a little more often than a solo player, and a new pair most: players win about 54% of ranked games with a partner they've never played with, against 50% alone.
> - To find a duo, look for a player within a rank of you, in your region, at the hours you play.

## Information gain

- **[UNIQUE INSIGHT] Who each rank can duo with, in one table.** rankforge.gg has examples; nobody lists every rank, Obscurus and Eternus included, or points out that an Ascendant can't duo with an Eternus at all.
- **[UNIQUE INSIGHT] The rules decide where and with whom to play.** Each follows from a confirmed rule:
  - Standard has no party limits and counts toward the 60 wins and the 15 per hero, so a group whose friends aren't eligible yet unlocks ranked together there. The LFG messages show players doing exactly that.
  - Calibration is solo and seeded from each player's own history, so last season's duo can come out apart. Players then throw games to fall back together; climbing is the way back that doesn't cost the higher player their rank.
  - Calibration tops out at Oracle VI. If a new period recalibrates everyone, a player who finished higher starts the period at Oracle VI or below, within duo reach of more friends than at the period's end. Use this only if the next period does recalibrate (see Timing).
  - A 6-stack never meets solo players, so the bigger the stack, the longer the wait.
  - Leaving a ranked game counts as a loss for the leaver's party too, so a duo partner who leaves costs you the game.
- **[UNIQUE INSIGHT] The lane setting.** None of the competitors mentions Lane preference. Valve added it in November 2024 and it's still in the client. Players report that it resets when the party breaks up or the game restarts, so check it before each session.
- **[DATA] What a duo is worth.** None of the competitors uses deadlock-api.com's study, and it answers what a player deciding whether to look for a partner wants to know: a duo wins little more than a solo player overall, the gap is largest at the top, and it comes from new pairs, not long-standing ones. The guide's one chart.
- **[CORRECTION] The first result.** Inven Global says ranked is solo only. The guide says otherwise, on Valve's own wording.

## Content outline

### Introduction (~120 words)

- **Hook:** The first answer Google gives for "can you play Deadlock ranked with friends" says no. Since July 2026 the answer is yes, with one friend, within a rank, once both have calibrated.
- **Who it's for:** a player who wants to play ranked with a friend, or has a group of three to six that ranked turns away.
- **Promise:** who can queue together, what a friend needs before they can, where a stack plays, how a duo shares a lane, how to find a duo partner, and where a full team goes. Link [every game's team format](/guides/join-an-esports-team#your-games-team-format) for other games.
- The TL;DR box goes here, as "In short", like the other game guides.

### H2: Who can queue ranked together

- **Answer first:** solo players, and duos at most one rank apart.
- Cover:
  - **Party size.** Ranked takes parties of one or two. A party of three or more can't queue for it.
  - **The rank gap.** "both players must be at most 1 Rank away from each other (i.e. Mystic and Ritualist)." Ranks, not subranks: Valve counts the eleven named ranks. One sentence that the subrank doesn't matter below Eternus, stated from Valve's wording.
  - **Eternus.** Eternus players duo only with another Eternus within three subranks, so an Ascendant and an Eternus can't duo.
  - **Valve may loosen this.** Valve says the limits "might be too restrictive" and that it will "adjust in the future as needed". One sentence, cited.
  - What the game shows when a duo is too far apart: "Your party contains too large of a badge range to play together in ranked." Stated as what the game shows, uncited.
- **Table: who each rank can duo with.** Two columns, twelve rows:
  - Obscurus (calibrating): nobody, calibration is solo
  - Initiate: Initiate, Seeker
  - Seeker: Initiate to Acolyte
  - Acolyte: Seeker to Sentinel
  - Sentinel: Acolyte to Mystic
  - Mystic: Sentinel to Ritualist
  - Ritualist: Mystic to Emissary
  - Emissary: Ritualist to Oracle
  - Oracle: Emissary to Phantom
  - Phantom: Oracle to Ascendant
  - Ascendant: Phantom, Ascendant
  - Eternus: Eternus, within three subranks
- Game cover beside the first paragraph, as in the other guides.

### H2: Before a friend can join you in ranked

- **Answer first:** each player unlocks ranked on their own, then plays eight calibration games alone. Until both have, the duo plays Standard.
- Cover:
  - **Unlocking ranked:** 60 wins in Standard, and at least three heroes with 15 Standard wins each. In ranked a player can only pick heroes they have 15 wins with.
  - **What the game shows** when a friend hasn't unlocked it: "Not all players in this party have unlocked ranked matchmaking and therefore this mode cannot be played." Uncited. This answers "why can't I play ranked with my unranked friend".
  - **Sanctions:** an active low priority penalty or comms ban keeps a player out of ranked, and so their duo.
  - **Calibration:** eight games, alone. Each player is seeded from their own past performance ("a soft reset"), and nobody leaves calibration above Oracle VI.
  - **Why friends calibrate apart.** Each carries their own history, so a duo can come out two ranks apart and lose the right to queue. One sentence that players see this, uncited. The way back is to climb, not to lose on purpose: a lost game costs the player who throws it, and the friend's rank still has to come within one.
- **Close with what this means for a group:** play Standard together while the newer friends unlock their heroes. Every Standard win counts toward the 60 and toward the 15 on the hero played.

### H2: Three to six friends: Standard

- **Answer first:** a stack plays Standard, the unranked mode, which has "no restrictions on party size or skill spread".
- Cover:
  - **Party size.** Up to a full team of six.
  - **Rosters.** A party of three or more needs four heroes in each player's roster instead of three. What the game shows when rosters overlap: "Your roster overlaps too heavily with other players in your party." Uncited.
  - **Mixed ranks.** When a party's skill is far apart, Valve matches it closer to its best player than to its average. Players in mixed groups report long losing runs. State the rule cited to the May 2024 notes, and that it has more to do with lobbies than with blame.
  - **Queue times.** A 6-stack never meets solo players, which Valve says may make its queue longer. Players report several minutes for a stack of three or more, more for a 6-stack or a stack with a high-ranked player in it. No numbers in the guide.
  - **Custom games** for a group of twelve: private lobbies, two teams of six. One sentence; it leads into the last H2.

### H2: Playing as a duo: lanes, voice and leaving

- **Answer first:** set the lane preference before you queue, agree on voice, and stay to the end.
- Cover:
  - **Lane preference.** In a party, Lane preference can be set from Balanced to With Party, which makes the game prioritise putting party members in the same lane "when possible", at some cost to balance, as the client warns. Players report it resets when the party breaks up or the game restarts, so check it each session. Where the setting sits in the client: confirm before writing (see "Not confirmed").
  - **Swapping lanes.** In ranked, players can swap lanes in the pre-game waiting area, not after. In Standard, a player can ask a teammate to swap lanes during the opening zipline phase.
  - **Voice.** Team voice is opt-in, with a prompt before the match. A party can set its chat to Party-only, All Team or Disabled, for a duo on Discord.
  - **Leaving.** A ranked game a player leaves counts as a loss for their party as well. The leaver also gets penalties after the first five minutes.
- Don't name duo hero combinations or lane roles.

### H2: Finding a duo partner

- **Answer first:** look for a player within a rank of you, in your region, at the hours you play, who plays the heroes you don't.
- Cover:
  - **Rank first.** Within one rank, or the duo can't queue. Players post their rank as two neighbours ("Sentinel/Mystic") for that reason. The table gives the window.
  - **Region.** Deadlock matches by region (North America, South America, Europe, Asia and Oceania) and picks the server with the best ping for the match. A partner on another continent plays on a bad ping. No region picker: don't tell players how to change region (see "Not confirmed").
  - **Heroes.** Each player can only pick heroes they've unlocked for ranked, so between you, cover enough heroes not to collide.
  - **Keep a good teammate.** After a good game, add your lane partner on Steam and ask them to queue again. Party invites go to Steam friends.
  - **What a duo is worth.** A short paragraph and the chart. deadlock-api.com studied 641,345 ranked matches from 30 July to 6 September 2026. Across them, duos won 50.9% against 50.0% for solo players. The gap is largest in the Ascendant and Eternus group, 53.8% against 49.8%, and at Initiate it runs the other way, 49.6% against 50.1%. What matters more is the partner: players won 54.2% with a partner they had no recorded games with before ranked launched, against 49.9% of their own solo games, while players whose usual partner made up more than 60% of their earlier games won 50.3% together and 51.2% alone. The study's own conclusion is worth quoting: "A friend you have never played with is worth about 4 points of win rate and an extra subrank or so per 100 games, while a partner you already have hundreds of games with is worth nothing measurable." Say that Valve doesn't record who queued together, so the study infers duos from friend lists, lobby slots and who plays with whom. Don't explain why new pairs win more: the study doesn't settle it, and it rules out new accounts. The advice the data supports is modest: a partner helps a little, and a new one more than an old one, so it's worth looking for one.
  - **The chart.** "How much more often a duo wins than a solo player", in percentage points, one bar per row, drawn from zero, extending right for a gain and left for a loss:
    - All ranks: +0.9 (50.9% in a duo, 50.0% alone)
    - Initiate: −0.5 (49.6%, 50.1%)
    - Ascendant and Eternus: +4.0 (53.8%, 49.8%)
    - Players with a new partner: +4.3 (54.2% with the partner, 49.9% of their own solo games)
    - Players with a long-standing partner: −0.9 (50.3%, 51.2%)

    Each row gives both win rates in its label or value, so the chart reads without the bars. The caption names the study, its author, its sample and period, and that duos are inferred. The last two rows compare the same players with and without their partner; the first three compare duo games with solo games, which is a different comparison, so the caption or a divider says so. The ranks from Seeker to Phantom are left out: the post gives them only in an image, as a gap of 0.5 to 2.0 points. Don't draw the two win rates as bars from zero, where 49% and 54% look the same. Don't cut the axis to make them differ either. The existing `.guide-chart` draws only shares from zero, so this needs a variant for bars on either side of zero, checked at 375 px and in both themes. Read the dataviz skill before building it.
  - **Agree before you queue:** voice, the hours, the lane preference, and playing the game out.
  - One line linking the make guide for keeping a group together, instead of repeating it: "How to make an esports team".
- **The TeamTavern step:** put up a player post with your rank, "Ranked" ticked under Looking for, and your Steam account as a contact, and say in the description which heroes you play. A group looking for its sixth puts up a group post. Then filter the posts by rank and message the ones within a rank of you.

### H2: From a stack to a team

- **Answer first:** Valve runs no tournaments or leagues; every competition is run by the community, in custom lobbies of six against six.
- Cover:
  - **The Death Slam League.** An open league for teams of six starters and up to two substitutes, played on Saturdays on North American servers, with no prize money. New teams start in the lowest division, seeded by the team's average rank, and move up and down between divisions. Link [dse.gg/dsl](https://dse.gg/dsl) as the source for its rules.
  - **Other events.** Invitationals and occasional open qualifiers, mostly in North America and Europe. Name none.
  - **Scrims.** Teams play scrims in custom lobbies against teams of a similar level.
  - Link the join guide's Deadlock section.
- **Keep it short.** This H2 needs a check at each rewrite: the leagues change faster than ranked.

### FAQ (5)

1. **Can you play Deadlock ranked with friends?** With one friend at most one rank away, once you've both unlocked ranked and played your eight calibration games. Three or more play Standard.
2. **Why can't I play ranked with my friend?** One of you hasn't unlocked ranked (60 Standard wins, three heroes with 15 wins each) or finished calibration, you're more than one rank apart, or one of you has a sanction. A party of three or more can't queue for ranked at all.
3. **Can three friends play Deadlock ranked?** No. Ranked takes solo players and duos. Three to six friends play Standard, which has no limits on party size or rank.
4. **Can you see your friends' rank in Deadlock?** Every player's rank shows at the start of a ranked match. Answer only that unless the profile check confirms more (see "Not confirmed").
5. **How do I get a friend into Deadlock?** While Deadlock is a playtest, access comes from friend invites: a player who has the game invites a Steam friend from the game's invite screen, and the friend gets access through Steam. Limited Steam accounts can't be invited. Delete this answer when Valve opens the game.

### Conclusion (~100 words)

- Takeaways: unlock and calibrate first, stay within a rank, set the lane preference, take a stack to Standard, and look for a partner a rank either side of you.
- **Call to action:** "Deadlock players and groups on TeamTavern".
- No Discord mention: unlike The Finals, Deadlock has no official Discord with LFG channels to name.

## Facts, with sources

Checked 2026-10-06.

**Valve's own pages. Cite these:**

- **Ranked, Standard, eligibility, calibration, party rules and leaving**, from Valve's [Matchmaking Update](https://store.steampowered.com/news/app/1422450/view/680756685198854910), 2026-07-30:
  - "You can now choose to play in Standard Mode (unranked), or the new Ranked Mode introduced in this update."
  - "Standard mode provides a lower-stakes way to play Deadlock with friends, without worrying about adjustments your Rank. There are no restrictions on party size or skill spread." (Valve's typo; quote it as is or paraphrase.)
  - "To be eligible to queue for Ranked Mode, you must have at least 60 wins in the Standard Mode. … the mode only allows you to pick heroes that you have won at least 15 games with in Standard Mode. You must have at least 3 heroes unlocked for Ranked to be able to have a valid sized roster to queue with."
  - "As the season starts, everyone will enter a calibration phase for 8 games. All players will have a soft reset of their prior Rank, maintaining some history of your previous performance relative to other players to help seed you initially into the calibration games. Players can only exit calibration at a max Rank of Oracle VI. You cannot be in a party during calibration matches."
  - "Ranked Mode will currently only allow for solo or duo parties. If you are in a duo party, both players must be at most 1 Rank away from each other (i.e. Mystic and Ritualist). However, Eternus players can only queue with another Eternus player as long as they are within 3 Subranks of each other. We recognize that the party size and rank spread limits might be too restrictive, but we wanted to start off this way and then adjust in the future as needed."
  - the ranks: Obscurus (uncalibrated), Initiate, Seeker, Acolyte, Sentinel, Mystic, Ritualist, Emissary, Oracle, Phantom, Ascendant, Eternus, "Each Rank has 6 Subranks within (I - VI)"
  - "You cannot queue for Ranked if you have any active sanctions on your account (low priority, comms bans, etc.)."
  - leaving before five minutes: "The game will count as a loss for the leaver and their party"; after: "that player will receive leaver penalties and their party will have the game count as a loss"; "Leaver penalties start with queue timeouts and escalate to increasing amounts of low priority games."
  - "You can also use the new pre-game waiting area to adjust your starting lanes. You won't be able to zipline swap after this."
  - "Once you enter a new Ranked Match, you will see a breakdown of the Ranks of all players in the game"
  - Season 1 "ending on October 7th, after which the following season will begin". Superseded; see Timing.
- **Lane preference, lane swaps and wide parties**, from Valve's [11-21-2024 update](https://forums.playdeadlock.com/threads/11-21-2024-update.47476/):
  - "When queuing as a party, you will have an option to override the default balance oriented lane assignment system and make the system prioritize putting your party members in the same lanes together."
  - "Added Swap Lanes feature. During the rooftop and zipline phase at the start of the game, you can now request to swap lanes with your teammates by clicking the button below their portrait on the scoreboard."
  - "If the range of skill in your party is too wide, the amount of MMR you can gain will be reduced somewhat based on the degree". This is for the matchmaking of that time, before Ranked Mode; leave it out.
  - The [12-06-2024 update](https://forums.playdeadlock.com/threads/12-06-2024-update.50599/) moved Lane Preference "(party only)" into a "Queue Options" button on the dashboard, which has since become the Hideout. The current client still has "Lane preference", "Balanced", "With Party", and "Prioritize starting in the same lane as party members when possible.<br><br>Note this may impact game balance."
- **Wide-gap parties in Standard**, from Valve's May 13, 2024 update (the forum thread is `05-13-2024-update.807`; mirror at [deadlock.wiki](https://deadlock.wiki/Update:May_13,_2024)): "in cases where the party has a large gap in skill, the party's overall skill will now be biased closer to the best player's values rather than the average." Cite the forum thread. It predates Ranked Mode, but nothing since has replaced it.
- **6-stacks**, from Valve's [09-12-2024 update](https://forums.playdeadlock.com/threads/09-12-2024-update.27974/): "A 6 player party will no longer match against solo players. This may result in longer queue times for 6 stacks."
- **Rosters for parties of three or more**, from Valve's [01-30-2026 update](https://forums.playdeadlock.com/threads/01-30-2026-update.102822/): "Parties of 3+ players now need to have an additional hero in their roster (from 3 to 4)".
- **Voice and party chat**, from Valve's [06-13-2024 update](https://forums.playdeadlock.com/threads/06-13-2024-update.5773/): "Voice/Text chat is now opt-in. There is a prompt pre-match for joining the chat"; "Parties can now set their in-game chat mode to be Party-only, All Team or Chat Disabled (if using discord or other programs)". The current client still has both.
- **Regions and servers:** Europe, Asia, South America and Oceania were added in June to August 2024; the [11-21-2024 update](https://forums.playdeadlock.com/threads/11-21-2024-update.47476/) lists leaderboards for "(North America, South America, Europe, Asia, Oceania)". The best-ping sentence in the [10-10-2024 update](https://forums.playdeadlock.com/threads/10-10-2024-update.36958/) describes the solo-only ranked mode of that time, which is gone, so the guide doesn't use it. It cites the 11-21 regions list only.
- **Custom games**, from Valve's [09-26-2024 update](https://forums.playdeadlock.com/threads/09-26-2024-update.33015/): "Added Custom Match play mode". Valve gives no player count; Dexerto (2024-09-27) gives "a total of 12 slots, six for each team". [City Never Sleeps](https://www.playdeadlock.com/cityneversleeps) (2026-09-29) added spectators to custom lobbies, so they still exist.
- **Playtest access**, from the [Steam store page](https://store.steampowered.com/app/1422450/Deadlock/), live 2026-10-06: "Access to Deadlock is currently limited to friend invites via our playtesters." Release date "To be announced". Steam friends only, from the [06-06-2024 update](https://forums.playdeadlock.com/threads/06-06-2024-update.4096/): "You must now be Steam friends with a person to invite them"; the friend must not be "a limited steam user". Access arrives through Steam, from the [08-01-2024 update](https://forums.playdeadlock.com/threads/08-01-2024-update.13369/): "the users will now receive access directly through Steam".
- **Valve on the game being unfinished**, from [City Never Sleeps](https://store.steampowered.com/news/app/1422450/view/694273194214819790), 2026-09-29: "despite the game being unfinished"; features "we want to be able to adjust and significantly change based on feedback before the game is fully released". Not for the guide's body; it explains why rules may still change.
- **The Death Slam League**, from [dse.gg/dsl](https://dse.gg/dsl) and its [rulebook](https://s.dse.gg/dsl-rulebook): six starters and up to two substitutes, main accounts only; new teams in Division 3, seeded by the team's average Statlocker rating; four divisions with promotion and relegation; Swiss best-of-three on Saturdays at 8 pm EST on NA Central servers; "Currently, there is no official prize pool." Checked 2026-10-06: the season under way began 2026-08-24 with 180 teams (the standings call it DSL 5); the next season's sign-up date isn't published, and the rulebook puts the off-season at four to eight weeks. Division 3 promotes sixteen and doesn't relegate; Premier, the top, relegates four. Solo players "can apply for team matching on the Player Portal". No rank requirement.

**deadlock-api.com's study. Cite it for the chart:**

- [Solo vs duo queue win rates in Deadlock ranked](https://deadlock-api.com/blog/solo-vs-duo-queue-ranked), Manuel Raimann, 2026-09-07: "every captured ranked match from launch on 30 July through 6 September 2026, minus players still in calibration: 641,345 matches and 248,636 players"; "Players queueing as a duo won 50.9% of their ranked games. Solo players won 50.0%"; "At Initiate, duos win slightly less than solos, 49.6% against 50.1%. From Seeker through Phantom the gap runs 0.5 to 2.0 points. In the Ascendant and Eternus group duos won 53.8% against 49.8%"; "Players from pairs with no recorded history won 54.2% with that partner and 49.9% of their solo games"; "players whose most frequent partner accounts for more than 60% of their pre-ranked games won 51.2% solo and 50.3% together"; "Valve does not tell us who queued together, so we infer duos from friend lists, lobby slots and co-play patterns."
- Also in the post, for the prose if there's room: a duo appears somewhere in 45% of games at Initiate, 58% to 69% from Seeker to Emissary, 79% at Oracle, 85% at Phantom and 80% in the Ascendant and Eternus group; "facing a duo without one costs you about one extra loss in a hundred games". The pairs figures by shared games (54.1% with no history, 49.4% past 500 games) compare different pairs, so the chart uses the same-player figures instead.

**From the client, through Valve's files.** State as what the game shows, never cited:

- "Players are only allowed to queue up for ranked as a Solo or Duo. Duo parties must be within 1 Rank up or down from each other. Eternus players can only queue with other Eternus players who are within 3 subranks of each other."
- "Your party contains too large of a badge range to play together in ranked."
- "Not all players in this party have unlocked ranked matchmaking and therefore this mode cannot be played."
- "Your roster overlaps too heavily with other players in your party. Try adding more heroes or changing the heroes in your roster"
- "You must complete 8 solo calibration games."
- the invite screen: "Have friends who you'd like to play with in our playtests? Click on their accounts below to invite them via Steam."; "This player is a limited Steam user, and therefore is not eligible for playing Deadlock at this time."

**From players' reports.** State as what happens in the game, without a link:

- **Calibration splits duos:** Silver 4 and Iron 3 in players' nicknames ([2026-08-02](https://www.reddit.com/r/DeadlockTheGame/comments/1vdqluf/)); a friend deranking to keep queueing ([2026-08-07](https://www.reddit.com/r/DeadlockTheGame/comments/1viccze/)).
- **Big stacks wait:** see Search intent.
- **Mixed groups in Standard lose:** "lost something like 13 of our last 16" ([2026-09-08](https://www.reddit.com/r/DeadlockTheGame/comments/1waetd7/)).
- **Lane preference resets** when the party breaks up, someone disconnects or the game restarts, and "doesn't always work" ([2026-05-02](https://www.reddit.com/r/DeadlockTheGame/comments/1t21jgm/), [2026-07-12](https://www.reddit.com/r/DeadlockTheGame/comments/1uuhre8/), [2026-02-09](https://www.reddit.com/r/DeadlockTheGame/comments/1r0b6m6/)).
- **The rank gap counts named ranks:** a player reports a duo of neighbouring ranks queueing at a low subrank of one and a high subrank of the other, seven subranks apart ([2026-08-05](https://www.reddit.com/r/DeadlockTheGame/comments/1vgmb8v/)). Backs up the reading of Valve's wording; the guide states the rule from Valve.

**Not confirmed, or sources disagree.** The guide handles each like this:

- **What happens on 2026-10-08.** See Timing. Until then the guide is not written.
- **Whether Lane Preference applies to a ranked duo.** Unconfirmed; the guide says Valve hasn't said, and points ranked duos to the pre-game lane swap. Where it sits is confirmed by [The Game Haus](https://thegamehaus.com/deadlock/how-to-lane-with-your-party-in-deadlock-duo-laning-guide/2026/03/23/) (2026-03-23: "open the cogwheel at the top of your screen. This will open the Party Settings menu") and a [Steam discussion reply](https://steamcommunity.com/app/1422450/discussions/0/591784375625046264/) (2026-03-02: Escape, then the icon near the party, then lane preference). Stated uncited, as what the game shows.
- **Rank on profiles.** Valve's 2024 note made rank history visible only to the player; nothing newer. FAQ 4 says only what the match shows.
- **A region picker.** None in the client; only an undocumented console command (`citadel_region_override`) that third-party guides pass around. Leave it out.
- **Whether a party from different regions can queue.** No Valve wording. Leave it out.
- **How long a playtest invite takes.** Only third-party figures (hours to two days). Leave the time out.
- **Rank points per game.** The Matchmaking Update says ±250 on average; a Valve developer on Discord and the game file say 300 since 2026-08-02. Not needed.
- **Street Brawl's party size.** Not published. Leave Street Brawl out.
- **Whether a stack is matched against stacks of its own size.** Valve says only that a 6-stack never meets solos.
- **Smurfing to play with friends.** Players warn that both accounts get banned; Valve has said nothing. Leave it out.
- **The study's weak spot**, which its author names: the Ascendant and Eternus group is a few hundred players who know each other, where the duo detection's error rate is unknown, and friend lists are read as they are now, not at match time, which favours pairs that keep winning together. The caption says duos are inferred; the guide doesn't need more than that.

## Adding TeamTavern's data

At launch the guide's only data is deadlock-api.com's. Once Deadlock has 50 or more posts, add a line giving the share of player posts that tick "Ranked", and, if there's room, a second chart of the ranks player posts give, in rank order, with the one-rank window marked. That's the question a player looking for a duo has. It's a meaningful rewrite, so change `updated`.

## Internal links

- **Links from this guide:**
  1. `/games/deadlock`, twice: in the duo partner H2 and in the conclusion. Anchor: "Deadlock players and groups on TeamTavern".
  2. The make guide, from the duo partner H2. Anchor: "How to make an esports team".
  3. The join guide, from the introduction ("every game's team format") and from the team H2 (its Deadlock section).
- **Links to this guide:**
  1. The join guide: a "Deadlock: community leagues" H3 under "Your game's team format", after Dota 2. 3–4 sentences: Valve runs nothing; teams are six with substitutes; the Death Slam League takes new teams in its lowest division, on NA servers; then a link here. A row in its format table: **Deadlock: the Death Slam League**, 6 players and up to 2 subs | Nothing stated: new teams start in Division 3 | Seasons, matches on Saturdays, in North America.
  2. The make guide: a row in its format table: **Deadlock: Ranked** | Nobody: a duo queues together, and the game fills the rest | Duos only, at most one rank apart, after each has calibrated alone.
  3. The `/guides` index.
  4. The Deadlock feed, once feeds link the guides that name their game, as the Marvel Rivals brief proposed.
- Editing the join and make guides adds a fact, so their `updated` changes too.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, and logo-512.png as `image`, as the other guides have. BreadcrumbList Home > Guides > How to play ranked with friends in Deadlock. `datePublished` and `dateModified`. No Person.
- Every rule the guide cites links to Valve's own page, on the words that name it, once per source per section. The client's messages are quoted as what the game shows. What comes from players (calibration splitting duos, stack queues, the lane setting resetting) is stated as what happens in the game, without a link. The chart's figures link to deadlock-api.com's study, their primary source. Nothing from the wiki, and no Reddit or Discord links.
- No "when we played" stories.

## Keeping it current

Valve calls Deadlock unfinished and says the party limits may change, so this guide goes stale faster than the others:

- **Each ranked season or period**, for party size, the rank gap, calibration and eligibility. Valve says future seasons will "likely have longer durations with splits in between" and "recency requirements on the heroes played"; the last could change the hero rule.
- **Each major update's notes** on Steam news and the forum changelog.
- **The public launch**, whenever it comes: delete FAQ 5 and anything that says "playtest".
- **The team H2**, at each rewrite.

Change `updated` only when a fact changes.

## Implementation

- `Server/Guide/deadlock-ranked-with-friends.md`, imported as text in `Guides.js`, and an entry in `Guides.purs`: slug, heading, title, description, dates.
- The Deadlock H3 and table row in `join-an-esports-team.md`, and the row in `make-an-esports-team.md`.
- The cover is already at `/images/games/400/deadlock.webp` once the build runs on the `deadlock` branch.
- The chart's variant for bars either side of zero, in `Client/Pages/Document.scss` beside `.guide-chart`, with each value in text for screen readers as the existing chart has.
- The specs that cover guides must still pass: one table per guide, heading ids for fragment links, and the rank table and the chart at 375 px.

## Timing

The user decided on 2026-10-06 to write the brief now and update it once the next ranked season starts. What that date is isn't settled:

- Valve's Matchmaking Update said Season 1 ends on 7 October.
- A Valve developer, Yoshi, wrote on Discord on 2026-09-28: "we are planning to extend the ranked season a little bit. … We'll follow up with an updated schedule soon." Seen only as a screenshot posted by @IntelDeadlock. No schedule has followed as of 2026-10-06.
- The game's `ranked_seasons.vdata`, changed on 2026-09-29, ends the first period of "Beta Season 1" at 2026-10-08 21:00 UTC and starts a second period of the same season then, with a new leaderboard and the same rules: 60 wins, 15 per hero, 3 heroes, 8 calibration games, party sizes 1 and 2.

The user decided on 2026-10-06 to publish the guide on 2026-10-07, before the check, with the rules as they stand, and to update it afterwards. So:

1. After 2026-10-08 21:00 UTC, read Steam news and the forum changelog, and check players' reports, for: whether a new season or period began, whether it recalibrates everyone, and whether party size, the rank gap, eligibility or the hero rule changed. If nothing has happened by then, check again when Valve posts the schedule.
2. Update this brief where a fact changed: the TL;DR, the table, the calibration insight (keep it only if the period recalibrates), the Timing section and "Not confirmed".
3. Update the guide where a fact changed, change its `updated`, and request indexing again.
4. Promotion waits for the public launch, as decided for the game.

## Distribution

- request indexing in Search Console on publish, and again after each rules rewrite
- link it from the join guide, the make guide and the guides index
- no Reddit posts before the public launch
- after 4–6 weeks, check which Deadlock queries reach the guide and which reach the feed. If a listing query ("lfg", "duo finder") lands on the guide, the guide is competing with the feed, so adjust its title.
