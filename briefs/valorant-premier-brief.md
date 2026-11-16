# Content Brief: How Valorant Premier works, and how to find a team for it

The seventeenth guide, and the second for Valorant after the ranked one. It
is a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md),
and its nearest model is the League of Legends Clash guide: an in-client
competition for a team, on a schedule, with divisions instead of tiers. It
covers what a player needs to know to play Premier with a team they chose:

- when Premier runs: Stages, the week's queue windows by zone, the playoffs
- how a team is placed: divisions, the provisional division, why the best
  player lifts it, and what locks after the first match
- Premier Score and the playoff bar, and what it takes to reach it
- what each player needs, and the roster rules: five to seven players, two
  matches a week each, no subs in the playoffs below Contender
- how to find the rest of the team, on TeamTavern and keeping it together

Stage V26A6 is in its fourth week: weekly matches run to Saturday 5 December
and the playoffs are on 5–6 December 2026, so a launch in the next two weeks
catches its searches. The next Stage will probably start with the new Act in
early January 2027, when the guide's schedule needs checking.

Prepared 2026-11-16. Evidence comes from:

- Riot's ten Premier help pages (dates below), saved as text in
  `%LOCALAPPDATA%/Temp/premier/`
- every 2026 patch's notes, 12.00 (6 January) to 13.10 (10 November), grepped
  for Premier (`premier/notes/`)
- Riot's [official wiki page on Premier](https://wiki.playvalorant.com/en-us/Premier)
  (`premier/wiki.txt`), for the Stage history; it lists V26A5 as current and has
  no V26A6 yet
- **the V26A6 schedule itself, from tracker.gg's Premier pages**, whose page
  data carries Riot's schedule for each zone, event by event, with local start
  and end times (`premier/tracker-premier-*.json`, read with `windows.mjs`)
- valorant.gs, which reads the game files: a V26A6 Stage exists with 15 events
  (7 scrim, 7 league, playoffs); no dates
- Google autocomplete for 30 Premier seeds (`gsc/suggest-premier.txt`) and the
  results page for "valorant premier times"
- Search Console, 20 October to 14 November (`gsc/now4-pq.tsv`)
- the local cluster's production copy, `team_tavern`, and the Valorant counts
  of the 21 October backup (`gsc/valorant-posts.txt`)

- Riot's [Act 6 Premier post on X](https://x.com/VALORANT/status/2110400239605895330),
  14 October 2026 (image saved as `premier/act6-x.jpg`)

Riot didn't announce V26A6 in any patch note: 13.08 (13 October) opens Act 6
without a Premier section, and nor do 13.09 or 13.10. It announced it on X
instead, as it does each Act ("This is your Act 5 Premier schedule", 18
August): the Act 6 post gives the seven weeks' dates and maps and the playoff
day, and says "Queue windows will differ by zone, find yours in the Premier
tab". It gives no points bar. tracker.gg's schedule fills in the windows and
agrees with the post's dates.

## Why this topic

The user chose it on 2026-11-16 from the next-guide check. GUIDES.md had held
it back until Riot posted a Stage's dates; the schedule is now readable from
Riot's data through tracker.gg.

- **Autocomplete is full**, mostly for times: "valorant premier times" (today,
  UK, EST, EU, Australia, India, Saturday, Sunday), "what time is valorant
  premier tonight / on Saturday / playoffs", "valorant premier schedule"
  (today, 2026, V26 Act 6, Act 6), "what days". Then how it works ("how does
  it work", "how many games", "how many matches per week", "how long", "how to
  join", "what do you win"), divisions and ranks ("divisions explained", "rank
  requirement", "rank disparity", "ranks in order"), points ("points system",
  "win points"), playoffs ("date", "format", "time", "rules"), requirements
  ("level requirements", "contender requirements"), console, and finding a
  team ("team finder", "teams to join", "how to find a premier team
  valorant").
- **Valorant is TeamTavern's biggest feed** ("valorant lfg" 3.6k impressions
  in four weeks), and the join guide draws "how to join a valorant esports
  team" variants.
- **What ranks is weak.** Google's first result is tracker.gg's schedule page;
  then Riot's Queues page ("around 7 PM local time, but this can vary between
  7-9 PM"), the Fandom wiki, Riot's Act 4 and Act 5 posts on Instagram and X,
  and schedule pages that say V26A6 is "TBA" (valorant-spind.de, updated 30
  October). The AI Overview says matches are "typically weekends or
  designated weeknights depending on your zone" and to "lock in your roster
  shortly before the queue window opens", "15–30 minutes ahead": vague, and
  the second is not a rule anywhere in Riot's pages.

## Template

**Recommended**: `how-to-guide`, shaped like `league-of-legends-clash.md`. The
reader wants to know when to be online, whether their team can enter and in
which division, what it takes to reach the playoffs, and how to fill the
places they're short of.

## Target keywords

- **Primary:** valorant premier, how does valorant premier work
- **Secondary:**
  - valorant premier times, schedule, what time is valorant premier, what days
  - valorant premier team size, how many players
  - valorant premier divisions, ranks, rank requirement, rank disparity
  - valorant premier points, premier score, playoff qualification
  - valorant premier playoffs date, format, time
  - valorant premier requirements, level requirement, console
  - how to join a valorant premier team, how to find a premier team
- **Leave to the feed:** "valorant premier team finder", "teams to join"
- **Leave out:** team names, tags and logos; map strategy and the week's map
  (it changes every Stage; Riot's schedule names it); rewards item by item;
  Challengers leagues beyond one sentence.

## Search intent

Informational with a day of the week in mind. The reader has a team, or part
of one, and wants to know when to be online, which division it will land in,
how many wins it needs, and who else can play.

## Information gain

- **[UNIQUE INSIGHT] The week, by zone.** In V26A6 every zone has one-hour
  match windows on Thursday and Saturday, and most on Sunday too, at about 19:00
  in the zone's own time; Japan and LATAM South at 21:00, Asia and Korea at
  20:00, South Asia at 20:30. Oceania, South Asia and the Middle East have no
  Sunday window. Practice windows sit on Wednesday, Thursday and Friday. The
  times stay on the zone's clock through daylight saving, so a reader in the
  zone always queues at the same hour. Nobody ranking gives this table; Riot's
  page gives "around 7 PM" and "at least two evenings per week".
- **[UNIQUE INSIGHT] Turning up beats winning.** A win is 100 points and a
  loss 25, and the bar is 600. A team that plays both matches every week of a
  seven-week Stage, 14 matches, reaches 600 with four wins (400 + 250); three
  wins and eleven losses is 575. A team that starts late needs six matches it
  can win: with three weeks left, it has to win all six. So start in the first
  week and never miss a week.
- **[UNIQUE INSIGHT] The first match locks the team.** Division and zone lock
  after the Stage's first match; after it, nobody can join whose MMR would
  move the division, and a player removed may not be able to come back. The
  division comes from the top five players' average MMR, weighted toward the
  best player when skills differ widely. So recruit, and pick the zone, before
  the first Thursday.
- **[UNIQUE INSIGHT] Seven players, but five in the playoffs.** Any of five
  to seven players can play a weekly match, and each player plays at most two
  a week across all teams. Below Contender, the five who queue for the first
  playoff match play every match of it, with no subs. The playoff queue window
  is 15 minutes, not an hour, and a team that misses it is out.
- **[UNIQUE INSIGHT] Requirements are light, but bans count.** SMS
  verification, one ranked placement in the account's lifetime, and no current
  ban or restriction, AFK ones included; a penalty during a Stage disqualifies
  the player for the rest of it. No rank minimum below Contender. PC only.
- **[FRESHNESS] 2026's Stages.** Seven weeks is the norm, but V26A2 ran six
  (bar 500) and V26A5 five (bar 450). V26A6 runs seven weeks, 21 October to
  5 December, with playoffs on 5–6 December.

## Content outline

### Introduction

- **Answer first:** Premier is Valorant's in-client league for a fixed team of
  five to seven, on PC. A Stage runs about seven weeks: each week a team plays
  up to two matches in one-hour windows around 19:00 in its zone's own time,
  on Thursday, Saturday and in most zones Sunday. 600 points reach the
  playoffs, a single evening's bracket. A win is 100 and a loss 25, so a team
  that plays every week needs only four wins.
- **Who it's for:** a player with a team, or part of one, who wants to play
  Premier with players they chose, or to join a team that does.
- **It covers:** when Premier runs, how a team is placed, Premier Score and the
  playoffs, what each player needs and the roster rules, and finding a team.
  Then the join guide link.
- In short (five or six bullets): team size and PC only; the week and the
  times; 100/25 and the bar, four wins in fourteen; the first match locks
  division and zone; requirements; finding players. Each bullet stands alone,
  with V26A6's dates where they're given.

### H2: When Premier runs

- Cover image. A Stage runs alongside an Act, about seven weeks of weekly
  matches and a playoff, by Riot's joining page. One map a week, which
  resets on Tuesday. Practice matches on the days between, which count for
  nothing.
- **The one table:** the V26A6 weekly windows by zone, in each zone's local
  time: Zone | Weekly matches | Playoffs. Zones with identical times share a
  row:
  - **US East, US West, Brazil, LATAM North, Northern Europe, Europe DACH,
    Europe France, Eastern Europe, Türkiye:** Thursday 19:00, Saturday 20:00,
    Sunday 19:00 | Sunday 19:00
  - **Europe IBIT:** Thursday, Saturday and Sunday 19:00 | Sunday 19:00
  - **Middle East:** Thursday and Saturday 19:00 | Sunday 19:00
  - **LATAM South:** Thursday, Saturday and Sunday 21:00 | Sunday 20:00
  - **Asia, Korea:** Thursday, Saturday and Sunday 20:00 | Sunday 20:00
  - **Japan:** Thursday, Saturday and Sunday 21:00 | Sunday 20:00
  - **Oceania:** Thursday 19:00, Saturday 20:00 | Sunday 19:00
  - **South Asia:** Thursday and Saturday 20:30 | Sunday 20:00
  Three columns fit a 375 px window; check on the phone spec, and fold the
  zone's time zone into the zone cell ("US East (Eastern Time)").
- Under the table: each weekly window is an hour, and a team can play both of
  its matches in one if the first finishes in time, but two on one map's
  window use up the week. The playoff window is 15 minutes. The times are this
  Stage's, as tracker.gg shows Riot's schedule; the Premier tab shows yours.
- No `<time>` elements: the windows are fixed on each zone's own clock
  through daylight saving (Europe's 19:00 was 17:00 UTC before 25 October and
  18:00 after), so a UTC time would be wrong for half the Stage. As in the
  Clash guide. **The user decides this; it is the main choice in the brief.**
- V26A6's dates, from Riot's Act 6 post: weekly matches 21 October to
  5 December 2026, the Playoffs Tournament on Sunday 6 December. (Contender
  and Invite's playoffs probably start on Saturday 5 December, by tracker.gg's
  schedule and 2026's earlier Stages; hedge or leave out.) Seven weeks is the usual length; 2026 also had a six-week Stage
  (bar 500) and a five-week one (bar 450), so check the patch notes and the
  Premier tab when a Stage starts.
- Contender and Invite: two weekly matches on one evening, Saturday, by
  Riot's Contender page and the schedule's two 15-minute Saturday windows;
  six weeks in V26A6, as in V26A3 and V26A4. One sentence; most readers are
  in Open to Elite.

### H2: How a team is placed: divisions

- Open 1–5, Intermediate 1–5, Advanced 1–5, Elite 1–5, Contender, Invite.
  The team gets a provisional division as soon as it's made, from the average
  Competitive MMR of its top five players, "adjusted to weigh the best
  player's MMR more heavily" when skills differ widely. Roster or zone changes
  move it until the first match, which locks it for the Stage.
- After the first match, nobody can join whose MMR would move the division,
  and a removed teammate may not be able to come back. So finish the roster,
  and pick the zone, before the first match.
- You only meet teams in your division and zone, all Stage. Weekly matches
  have rematch protection (unless a queue runs past five minutes).
- Promotion: win your division's playoff bracket, from Open to Elite; the
  promotion counts only if you play the next Stage. No demotion for losing;
  a weaker roster or a dip in Competitive can place the team lower next
  Stage. Contender needs Contender Eligibility: in this Stage or the last, a
  match in Contender or Invite, Immortal 3 peak, or a playoff match in
  Elite 5. Invite's top teams can earn a place in a Challengers league; one
  sentence, and the join guide covers it.
- Link the ranked guide by its H1 for a five that also queues Competitive,
  where a five-stack can be any ranks.

### H2: Premier Score and the playoffs

- 100 points for a win, 25 for a loss, 100 for a bye (no opponent found).
  Playoff matches give none. 600 qualifies, whatever the standings; higher
  scores get better seeding.
- The arithmetic: 14 matches in seven weeks; four wins reach 650, three reach
  575. Six straight wins also reach 600, so a team that starts in week five
  must win them all. "If you join too late … there may not be enough match
  opportunities" (Riot).
- The playoffs: a single-elimination bracket of up to eight teams from your
  division, played in one evening from a 15-minute queue window; miss it and
  you're out. Bo1 with map pick and ban; the party leader locks in the team's
  choices. Below Contender, no subs: the five who queue for the first match
  play every match. Contender and Invite play across two days and can sub
  between rounds from the roster as it stood when playoffs began.
- What winning gives, briefly: promotion, a title and gun buddy, the
  Champion's Aura; a player card for anyone who played one match. Link
  Riot's rewards page.

### H2: What each player needs, and the roster

- **Requirements:** SMS verification, kept throughout; at least one ranked
  placement in the account's lifetime; no current ban or restriction, AFK
  ones included. A penalty during a Stage disqualifies the player for the
  rest of it, though they stay on the roster. Competitive opens at account
  level 20, so a new account reaches 20 and plays its placements first (link
  the ranked guide). PC only.
- **The roster:** five to seven players, each on one team at a time. The
  owner makes the team (name 5–15 characters, tag 1–5, fixed once the Create
  Team window closes) and can name one captain; both invite and remove. The
  party leader queues the team and locks in picks and bans; the owner needn't
  play.
- **Two matches a week, per player.** A player who plays two for one team and
  then switches can't play again until the week resets on Tuesday. So a sixth
  and seventh player are what keep a team playing every week.
- **Team tag:** shows after two weekly matches or one playoff match; one line.

### H2: Finding a team for Premier

- Recruit for the Stage: your zone, the evenings you can make, a rank close
  to the team's (it decides the division and, after the first match, who can
  join), and the roles you're short of.
- **Before the first match.** Fill the roster in the week before the Stage
  starts; after the first match, only players who don't move the division
  can join.
- **Six or seven, not five.** With two matches a week per player and no subs
  in the playoffs below Contender, a team of five stops whenever one can't
  play. Recruit a sixth for the weekly matches, and be sure of your five for
  playoff night.
- **Roles.** A five usually wants a Duelist, an Initiator, a Controller and a
  Sentinel, as in the ranked guide; say which you're short of, and whether
  you need someone to make the calls.
- **The same evenings.** Thursday and the weekend, at your zone's hour, every
  week for seven weeks.
- **Keep a good team** for the next Stage: team, history and Premier MMR carry
  over; the make guide covers keeping a roster together.
- **Post on TeamTavern:** a player post with your "Rank", "Role", "In-game
  leader" if you make the calls, "PC" under "Platform", and "Premier" ticked
  under "Looking for", with Riot ID and Discord as contacts. A team short of
  players puts up a group post with a rank range around its top five, the
  roles it's missing and its zone in the description. Filter for "Premier"
  and message the posts that fit. No counts: "Premier" is new in "Looking
  for", and only a handful of posts tick it.
- Feed link: `[Valorant players and groups on TeamTavern](/games/valorant)`.

### FAQ

1. **What time is Valorant Premier?** Around 19:00 in your zone's own time,
   in one-hour windows on Thursday, Saturday and in most zones Sunday; 20:00
   to 21:00 in Asia, Korea, Japan, South Asia and LATAM South. The Premier tab
   shows your zone's.
2. **What days is Valorant Premier on?** Thursday, Saturday and Sunday in
   most zones in V26A6; Thursday and Saturday in Oceania, South Asia and the
   Middle East. Playoffs on the Stage's last Sunday.
3. **How many matches do you play in Premier?** Up to two a week, for each
   team and each player, for seven weeks: 14 in a full Stage, then the
   playoffs.
4. **How many points do you need for Premier playoffs?** 600 in a seven-week
   Stage. Wins are 100 and losses 25, so four wins in 14 matches is enough.
   Shorter Stages have had lower bars (500, 450 in 2026).
5. **How many players are on a Premier team?** Five to seven; five play each
   match.
6. **Is there a rank requirement for Premier?** No, below Contender. The team's
   division comes from its top five's MMR, weighted toward the best. Contender
   and Invite need Contender Eligibility, such as an Immortal 3 peak.
7. **Can you play Premier on console?** No. Premier is PC only.
8. **When are Premier playoffs?** On the Stage's last Sunday: in V26A6,
   Sunday 6 December 2026, by Riot's schedule, in a 15-minute queue window
   at about 19:00 in your zone's time.
9. **Can you join a Premier team mid-Stage?** Yes, at any time, but after the
   team's first match only if your MMR doesn't move its division, and late
   joiners may not have enough matches left to reach 600.

### Closing H2: Find your team

Bullets: make the team and pick the zone before the first match; six or seven
players, so a missed evening doesn't cost the week; play every week, since
four wins in fourteen reach 600; five sure players for playoff night. Then the
call to post and the feed link, and Valorant's official Discord if it is one
(check before writing; the 12.11-era "Exclusive Discord Drop" post suggests
an official server).

## Facts, with sources

Checked 2026-11-16.

**Riot's help pages** (each page's own date):

- [Team Creation & Management](https://support.riotgames.com/en-us/valorant/gameplay/premier-team-creation-management),
  7 Jul 2025: owner (transfer any time; passes to the longest-serving member);
  owner invites/removes and names "one player as the Team Captain"; "the
  party leader queues the team for matches and locks in the team's choices
  for map picks and bans"; name 5–15 characters, tag 1–5, "You can't change
  your team name and tag once you leave the Create Team window"; "Premier
  teams have 5-7 players. Don't worry about MMR disparity—anyone can join and
  your provisional division … will be determined by your top 5 players'
  average MMR"; one roster at a time; a penalty makes a player ineligible "for
  30 days from the start of the penalty"; zone "locked in after your first
  match"; "If there's a big difference in skill, then you may be placed in a
  higher division to give more weight to your best player's MMR"; "BE CAREFUL
  WHEN YOU REMOVE A TEAMMATE AFTER YOUR FIRST MATCH"; "one map per week";
  "Both teams and individuals are limited to 2 matches per week"; practice
  matches don't count; "No enrollment necessary!"
- [Joining & Leaving Teams](https://support.riotgames.com/en-us/valorant/gameplay/premier-joining-leaving-teams),
  30 Mar 2026: "A Stage is one Premier tournament lifecycle, which roughly
  coincides with an Act"; "Up to 2 matches per week, for 7 consecutive weeks
  (plus 1 more week if your team makes the Playoff Tournament)"; SMS
  verification kept throughout; "at least one ranked placement in your
  account's lifetime"; "no current bans or restrictions of any kind … that
  includes AFK restrictions"; bans during Premier disqualify "from the rest of
  that stage"; "If you join too late, however, there may not be enough match
  opportunities"; leave any time except in queue; a Crest needs two weekly or
  one playoff match.
- [Divisions](https://support.riotgames.com/en-us/valorant/gameplay/premier-divisions),
  24 Jun 2025: the six divisions; provisional division from the top five's
  Competitive MMR, recalculated on roster or zone change, "adjusted to weigh
  the best player's MMR more heavily"; "Once your team has played a match in
  the current Stage, your team cannot change divisions"; promotion by winning
  the playoff bracket, at least one division up, only for the next Stage;
  "Teams are not demoted based on performance."
- [Contender & Invite Divisions](https://support.riotgames.com/en-us/valorant/gameplay/premier-contender-invite-divisions),
  22 Dec 2025: Contender Eligibility (current or previous Stage: a match in
  Contender or Invite, Immortal 3 peak, a playoff match in Elite 5); queue
  "waiting period is 15 minutes"; "one evening per week with two Weekly
  matches"; 0 points for a loss in these divisions; playoffs Bo1, finals Bo3,
  over several days, subs between rounds; top Invite teams "earn the
  opportunity to make it into a Challengers League".
- [Playoff Tournaments](https://support.riotgames.com/en-us/valorant/gameplay/premier-playoff-tournaments),
  22 Dec 2025: "Earn at least 600 Premier Score to qualify for playoffs";
  "up to 8 teams, but may start with as few as 2"; "If your team loses even
  once in a tournament, you're eliminated. If your team misses the tournament
  queue window, you will not be able to participate"; map pick and ban, party
  leader locks in; "up to Elite, we do not currently support subs during
  tournaments. The 5 players that queue up for the first match … will be
  locked in"; Open to Elite matches "played consecutively in one day", at
  least 5 minutes' rest.
- [Queues & Matchmaking](https://support.riotgames.com/en-us/valorant/gameplay/premier-queues-matchmaking),
  24 Jun 2025: "Queue windows are unique to each zone … Most Weekly matches
  occur around 7 PM local time, but this can vary between 7-9 PM"; "within 8
  minutes"; "one-hour window"; "at least two evenings per week … and playoffs
  on the last day of the Stage"; same division and zone; rematch protection
  "unless you're in queue for 5+ minutes".
- [Weekly Matches](https://support.riotgames.com/en-us/valorant/gameplay/premier-weekly-matches),
  24 Jun 2025: weekly map "resets on Tuesday"; two matches in one window if
  time allows, but "If you play two matches on Thursday on Bind, you won't be
  able to play on Saturday"; practice matches in a one-hour window on
  non-match days.
- [Team Hub & Score](https://support.riotgames.com/en-us/valorant/gameplay/premier-team-hub-score),
  24 Jun 2025: Open to Elite: "100 points for a Weekly match win, and 25
  points for a loss. Tournament matches do not award points."
- [Rewards & Perks](https://support.riotgames.com/en-us/valorant/rewards/premier-rewards-perks),
  12 Jan 2026: tag and plating after 2 weekly or 1 playoff match; Champion's
  Aura; a Stage player card for one match; title and gun buddy for playoff
  winners.

**Riot's patch notes, 2026:** [12.00](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-12-00/)
V26A1 from 14 January; [12.05](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-12-05/)
V26A2 from 18 March, "six weeks of matches instead of the usual seven", bar
500, playoffs 26 April; [12.08](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-12-08/)
V26A3 from 6 May, Contender and Invite six weeks of Saturdays, playoffs over
two days; [13.00](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-13-00/)
V26A4 from 1 July, bar 600, playoffs 16 August (Contender and Invite 15–16);
[13.04](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-13-04/)
V26A5 from 18 August, "five weeks of matches instead of the usual seven", bar
450, playoffs 20 September; [13.08](https://playvalorant.com/en-us/news/game-updates/valorant-patch-notes-13-08/)
opens Act 6, no Premier section. Each Premier section sits under "PC ONLY".

**Riot's [official wiki](https://wiki.playvalorant.com/en-us/Premier):**
"available only on PC"; "The Weekly Matches phase lasts for seven weeks";
"Each day, a 1-hour queue window opens in the evening for the team's local
time zone"; 100/25; 600; V25A6 ran 22 October to 7 December 2025. Its history
of changes: bar 375 → 675 at launch, 675 → 600 in 2024; playoff window 1 hour
→ 15 minutes.

**Riot's [Act 6 post on X](https://x.com/VALORANT/status/2110400239605895330),
14 October 2026:** "Your next run starts here. Make your mark in Act 6
Premier." The image: Stage V26A6, "October — December"; Week 1 Bind (21–25
October), Week 2 Corrode (28 October – 1 November), Week 3 Sunset (4–8
November), Week 4 Haven (11–15 November), Week 5 Summit (18–22 November),
Week 6 Abyss (25–29 November), Week 7 Lotus (2–5 December); Playoffs
Tournament 6 December; "Queue windows will differ by zone, find yours in the
Premier tab". Cite it for the dates; it names no bar and no times. Riot posted
the same kind of image for Acts 2, 4 and 5 of 2026 and Act 6 of 2025.

**The V26A6 schedule, from tracker.gg's page data** (Riot's schedule per
zone, read 2026-11-16): Stage 13 October 2026 to 5 January 2027; first
events 21 October; one-hour Match windows on Thursday (7), Saturday (7) and
Sunday (6) at the times in the outline; 15-minute Match windows on Saturday at
19:00 and 21:00 (6 each; 20:00 and 22:00 in Japan and LATAM South, 19:30 and
21:30 in South Asia); one-hour Scrim windows on Wednesday, Thursday and
Friday; Tournament windows of 15 minutes on Saturday 5 December (19:00 and
21:00 local) and Sunday 6 December (19:00 local; 20:00 in Asia, Korea, Japan,
South Asia and LATAM South). Times stay on the zone's clock across daylight
saving (EU DACH +02:00 on 21 October, +01:00 from 1 November).

**Search Console**, 20 October to 14 November: Valorant feed 11.8k
impressions ("valorant lfg" 3.6k at 7.6); the ranked guide 18.2k; no Premier
query reaches the site yet.

## Not confirmed

- **V26A6's playoff bar.** Riot's playoff page and wiki give 600; 2026's short
  Stages had lower bars, announced in the patch notes, and V26A6 is the usual
  seven weeks with no note. Say 600 "for a seven-week Stage" and that the
  Premier tab shows the Stage's.
- **Which windows belong to which divisions.** tracker.gg's zone schedule
  doesn't label them. The 15-minute Saturday pairs are taken to be Contender
  and Invite (Riot: 15-minute waiting period, "one evening per week with two
  Weekly matches"; six of them, as Riot's notes gave Contender six weeks in
  V26A3 and V26A4), and the one-hour windows Open to Elite. Say it in those
  terms, or leave the 15-minute windows out of the table.
- **Whether other Stages keep these windows.** Only V26A6's times were read.
  Riot's "around 7 PM … 7-9 PM" fits, and the table says it's this Stage's.
  Recheck at V27A1.
- **The zones' names in the client.** tracker.gg's labels (US East, Northern
  Europe, Europe IBIT, Türkiye …) are used; check them against a screenshot of
  the zone picker if one turns up.
- **Contender and Invite's playoff days.** Riot's post gives one playoff day,
  6 December; tracker.gg's schedule also has 15-minute tournament windows on
  Saturday 5 December, which fit the two-day Contender and Invite playoffs of
  V26A3 to V26A5. Say "6 December" for Open to Elite, and leave Contender's
  days to a hedged sentence or out.
- **Timeouts.** Riot's Weekly Matches page says two a team, the wiki one that
  refreshes at half time; left out.
- **An official Valorant Discord.** Check before writing.

## Adding TeamTavern's data

None. "Premier" is new in "Looking for" since the relaunch: 3 player and 3
group posts tick it in the 21 October backup. No chart; recount when the
option has been out for a few Stages.

## Internal links

- **From this guide:** `/games/valorant` twice (the recruiting H2 and the
  close); the join guide from the introduction
  (`/guides/join-an-esports-team#your-games-team-format`); the ranked guide by
  its H1 ("Valorant team size, party size and playing ranked with friends")
  from the divisions H2 and the requirements H2; the make guide from the
  recruiting H2.
- **To this guide:** the join guide's "Valorant: Premier" H3 and the ranked
  guide's Premier paragraph under "Team size and party size" and its "From
  ranked to a team" bullet, which now point at the join guide's Premier
  section.

## Adding to the join and make guides

- **Join guide, table row:** keep "**Valorant: Premier**, 5–7 players | SMS
  verification, one ranked placement ever, and no current bans or
  restrictions"; change the last cell to "Stages of about 7 weeks, up to 2
  matches a week, then a playoff evening" if it fits.
- **Join guide, the Premier H3:** keep it; add that the weekly windows are
  Thursday and the weekend around 19:00 in the zone's own time, and that four
  wins in a full Stage reach the playoffs, and link this guide by its H1.
- **Make guide:** its row and its "Premier punishes late changes" paragraph
  already match; link this guide from the paragraph.
- **Ranked guide:** point the Premier paragraph and "From ranked to a team" at
  this guide by its H1, and move its `updated`.
- Move the join and make guides' `updated` only where a sentence changes.

## Slug, title, description

- **Slug:** `valorant-premier`
- **H1:** How Valorant Premier works, and how to find a team for it
- **Title:** Valorant Premier: times, divisions, points and how to find a team
- **Description:** A weekly league for a team of five to seven, two matches a
  week in your zone's evening and a playoff for 600 points: how Valorant
  Premier works, when to be online, and how to find a team for it.

## Keeping it current

- **When the next Stage starts** (probably with the new Act in January 2027;
  V26A1 started 14 January): Riot's schedule post on X (search
  `from:VALORANT premier` with `f=live`; the signed-in playwright-chrome
  loads it), the windows from tracker.gg as above, and the patch notes for its
  length and bar. The table, the V26A6 dates in the
  In short box and the FAQ, and the playoffs answer. The slug outlives the
  Stage.
- **When a Riot help page's date moves:** compare its paragraph.
- Change `updated` only when a fact changes.
