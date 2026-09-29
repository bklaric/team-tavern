# Content Brief: How to join an esports team

The first guide in `/guides`. It is evergreen: it keeps its URL and shows an "Updated" date that changes whenever a fact below changes.

Prepared 2026-09-28. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, Jun 2025 to Sep 2026
- the current results for the target query
- official publisher pages, with each one's date given where it's cited
- TeamTavern's own posts in the development database, a copy of production taken 2026-09-26

## Template

**Recommended**: `how-to-guide`. The query asks for steps. The pages that rank now are loose lists of generic steps (practise, stream, network, apply). This guide gives steps that name each game's actual way in.

## Target keywords

Search Console reports impressions, not search volume. The impressions below are for TeamTavern's home page, which ranks for these queries without answering them.

- **Primary:** how to join an esports team. 45.7k impressions at position 8.9, 18 clicks.
- **Secondary:**
  - how to get on an esports team: ~550 across its spellings, position ~9.8
  - join esports team: 1.1k, position 9.2
  - how to join a pro gaming team: 278, position 10.2
  - esports team tryouts: 381, position 12.5
  - how do I join an esports team: 372, position 9.6
- **Leave to the feeds:** "esports teams to join" (1.7k), "small esports teams to join" (1.6k) and "esports teams looking for players" (676). These people want listings, not a guide. The guide should send them to the feeds, not try to rank in their place.
- **Questions:**
  - What rank do you need to join an esports team?
  - How do esports tryouts work?
  - How do I find a team to scrim with?
  - Can you join an esports team at 16?
  - Do amateur esports teams pay players?
  - How do I join a college esports team?

## Search intent

The intent is informational, and the readers are nearly all amateurs. They play ranked, they're good at it or think they are, and they want to play on a team instead of queueing alone. Most of the pages that rank write for someone chasing a pro contract. The real next step for these readers is a five-stack playing their game's own team tournament and scrims, and none of those pages says so.

## Content parameters

- **Word count:** 2,200–2,800. Competitors run 1,200–3,200. The per-game section carries the length, and every other section stays short.
- **Reading level:** plain and direct, like the site's About page. The copy says "we" or uses the passive, never "I".
- **Format:** a page rendered like About and Terms, through `document { title, updated }`, with the added H2s and table the guide needs.
- **H2 sections:** 7, plus an FAQ.
- **Images:**
  - the game covers in the per-game section, which the site already has
  - one chart from TeamTavern's own data
  - one table
  - no stock photos
- **FAQ:** 5 questions. They are there for readers and AI answers. Google stopped showing FAQ rich results, so they win nothing in the results page.

## Recommended title

**How to join an esports team: the way in for Valorant, League, CS2 and more**

Alternatives:

1. How to join an esports team (when you're not pro yet)
2. How to get on an esports team: formats, tryouts and where teams recruit

The H1 can be shorter: "How to join an esports team".

## Meta description

Most esports teams are amateur five-stacks playing Premier, Clash, ESEA or Siege Cup. Here's what each game asks of your account, what teams look for, and where to find one.

(165 characters, so Google may cut the last few words.)

## TL;DR draft

> Almost every esports team you can join today is an amateur stack playing its game's own team tournament (Valorant Premier, League Clash, Dota Battle Cup, Siege Cup) or a league such as ESEA, plus scrims. Get your account eligible, and pick the role teams are short of. Then post your rank, role and availability, and answer the group posts. On TeamTavern there are about ten player posts for every group post, so waiting to be found is the slow route. Pro teams recruit from the top of these same ladders.

## Information gain

Nothing ranking now has either of the first two.

- **[ORIGINAL DATA] Supply and demand.** TeamTavern's posts from 2019 to 2026 are 26,831 player posts and 2,538 group posts, about 10.6 players for every group.
- **[ORIGINAL DATA] The roles teams are short of.** For each role: the share of group posts asking for it, next to the share of player posts offering it (both counted only among posts that answered the role field):

  | Game | Role | Groups asking | Players offering |
  |---|---|---|---|
  | CS2 | Support | 68% | 38% |
  | CS2 | Lurker | 63% | 39% |
  | CS2 | Entry | 68% | 46% |
  | Dota 2 | Mid | 67% | 40% |
  | League | Top | 58% | 36% |
  | League | Jungle | 57% | 38% |
  | Siege | Anchor | 78% | 63% |
  | Overwatch | Tank | 43% | 41% |
  | Overwatch | Support | 71% | 71% |

  The CS2, Dota and League rows show real gaps. The Overwatch rows are nearly level, and that's worth saying too.
- **[ORIGINAL DATA] How often groups want scrims or tournaments.** Share of group posts that tick "Scrims and tournaments":

  | Game | Share |
  |---|---|
  | League | 62% |
  | Dota 2 | 60% |
  | CS2 | 54% |
  | Valorant | 48% |
  | Siege | 41% |
  | Apex | 35% |
  | Overwatch | 21% |

  A reader can take from this that in League or Dota, competitive teams are the usual case, and in Overwatch they are the exception.
- **Caveats for the data:**
  - Roles are multi-select, so the shares don't add up to 100%.
  - Posts from the old site came through the relaunch import's mapping.
  - **Leave Valorant roles out.** The import mapped only "entry fragger" (to Duelist), so every Valorant role reads as Duelist.
  - Leave TF2 and HotS out: they have 12 and 2 group posts.
  - The queries are reproducible from the `post`, `post_field_option`, `field_option` and `field` tables. Rerun them against production before publishing and put the date beside the numbers.
- **[UNIQUE INSIGHT] A requirements matrix.** Team size, what each game asks of your account, what it costs, and how often it runs, for every game on the site. None of the competitors has this, and it's the part readers will bookmark.
- **[UNIQUE INSIGHT] The ladder in each game.** Name the ladder that actually leads from amateur to pro:
  - Valorant: Premier Invite, then Challengers
  - CS2: ESEA, then ESL Challenger League, then Pro League
  - Overwatch: FACEIT League, then OWCS
  - Apex: Challenger Circuit, then Pro League
  - Siege: Challenger Series, then the regional league

  Say plainly that only a handful of teams per region go up each year.

## Content outline

### Introduction (~150 words)

- **Hook:** "Joining an esports team" sounds like tryouts at a pro organisation. For almost everyone it means finding four people at your rank who show up on the same night each week.
- **Problem:** The guides that come up for this are careers pieces. They tell you to stream and network, and never name the tournament you'd actually play.
- **Promise:** What each game's team format is and what it asks of your account, the role teams are short of, how to get picked, and how the amateur ladder leads to pro.
- The TL;DR box goes here.

### H2: What an esports team is below pro level

- **Answer first:** An amateur team is a group of 5 (3 in Apex, 6 or 9 in TF2) that plays together on a fixed schedule, in its game's team tournament, a league, or scrims.
- Cover:
  - scrims, defined in one sentence: practice matches against another team
  - the difference between a stack that plays ranked, a tournament team, and a league roster
- **Key stat:** the scrims shares from the Information gain section.

### H2: Pick one game and one role, ideally the role teams are short of

- **Answer first:** Teams recruit for a gap in the roster, not for a good player in general.
- Cover:
  - the roles that are scarce in each game, from the supply-and-demand table
  - in-game leader, which Valorant, CS2, Siege and Apex groups recruit for by name
- **Chart:** grouped bars per game and role, groups asking against players offering, for CS2, Dota 2, League, Siege and Overwatch. Source it to TeamTavern's posts with the date.

### H2: Make your account eligible

- **Answer first:** Every official format has a gate. Most are free, and some cost money.
- **Table: the requirements matrix.** Rows from Part B of the facts below, one per game. Columns: format, team size, account requirements, cost, schedule.

### H2: Your game's team format

One H3 per game, in the order of TeamTavern's busiest feeds. Each H3 has 2–4 sentences, then a link to the game's feed with anchor text "<Game> players and groups on TeamTavern". It also links the sibling guide once that exists.

- **Valorant (Premier):**
  - Create or join a team in the Premier tab. It's free, and a team can start at any point in a Stage.
  - Teams have 5–7 players, play one evening a week, and finish each Stage with playoffs.
  - Divisions run Open, Intermediate, Advanced, Elite, then Contender and Invite.
  - Invite is the door to Challengers.
- **League of Legends (Clash):**
  - Twelve cups in 2026, alternating Summoner's Rift and ARAM.
  - Eight-team single-elimination brackets, tiers I–IV.
  - You need level 30, ranked placements, SMS verification and a ticket.
  - Clash doesn't lead to pro play; scrims and third-party leagues do.
- **Overwatch:**
  - The game has no in-game LFG.
  - The open path is FACEIT League, and its top teams play promotion matches against the bottom OWCS teams.
- **Dota 2 (Battle Cup):**
  - A weekend eight-team bracket for five players, free with Dota Plus.
  - Keep this H3 short, since most of its details aren't confirmed (see below).
- **Counter-Strike 2:**
  - Premier is matchmaking, not a team format.
  - Teams play ESEA (Open to Advanced) or FACEIT.
  - Since November 2025 the upper ESEA divisions count toward the Valve Regional Standings (VRS), and 2 teams per region reach ESL Challenger League.
  - The Majors take teams by VRS invite only; there are no open qualifiers any more.
- **Apex Legends:**
  - Trios.
  - The ALGS Challenger Circuit is open entry through Battlefy, with your EA account linked.
  - Up to 1,280 teams per region per event, and the Circuit leads to the Pro League.
- **Rainbow Six Siege:**
  - Siege Cup is the in-game five-stack tournament.
  - Since Siege X, Ranked and Siege Cup need the paid game.
  - The open Challenger Series on Challengermode sends both grand finalists into the regional league.
- **Team Fortress 2:**
  - Competitive play is ETF2L in Europe and RGL in North America, in 6s and Highlander.
  - ETF2L has a Fresh division for new players.
  - Mann Up is co-op for 6, not competitive.

### H2: Find a team: post, then answer posts

- **Answer first:** Put up a player post that says your rank, role, whether you can lead, your region, languages and the hours you're online, and tick "Scrims and tournaments" or your game's format. Then message the group posts that fit you.
- Cover:
  - With about ten players for every group, answering posts works faster than waiting to be found.
  - A microphone matters, because teams ask for voice.
  - TeamTavern emails you when a new post fits yours.
- Link the sibling guide "Writing an LFG post that gets answers".

### H2: What tryouts look like

- **Answer first:** An amateur tryout is usually a voice call and a few scrims, sometimes a trial week.
- Cover:
  - What teams look for: showing up on time, comms, taking criticism, fitting the schedule.
  - Treat the team the same way: ask when they play, and who leads.
  - A short paragraph on college teams. US college programmes hold tryouts each term. NACE has grown from 42 member schools in 2018 to over 200 by late 2022; cite the date as given.

### H2: From amateur team to pro

- **Answer first:** Pro teams recruit from the top of the ladders above, and each ladder lets only a few teams up each year.
- Cover:
  - the ladders listed under Information gain
  - Riot says Premier sent over 40 teams into Challengers in 2024
- **Diagram (optional):** one row per game, left to right: open format → tier 2 → tier 1.

### H2: Or start your own team

- Two sentences, then a link to the sibling guide "How to make an esports team".
- Put up a group post and name the roles you're missing.

### FAQ (5)

1. **What rank do you need?**
   - No universal minimum.
   - Premier's upper divisions need an Immortal 3 peak (or equivalent results).
   - Most amateur teams want someone within a few divisions of themselves.
2. **How do esports tryouts work?** One paragraph, from the tryouts section.
3. **Can you join at 16?**
   - TeamTavern's minimum age is 16.
   - Tournaments set their own ages, so check the rulebook.
   - Don't state other organisers' ages; they're unconfirmed.
4. **Do amateur teams pay?**
   - Almost never.
   - Prize money exists in the upper circuits; the EML Challenger Circuit's pool is €75k across the year.
5. **How do I find a team to scrim with?** Groups on TeamTavern tick "Scrims and tournaments", and the feed filters for it.

### Conclusion (~120 words)

- Takeaways: format, eligibility, scarce role, post and answer, tryout.
- **Call to action:** "Find your team" with a link to each busy feed, and the home page for the rest.

## Statistics to include

| # | Statistic | Source | Date | Section |
|---|---|---|---|---|
| 1 | 26,831 player posts against 2,538 group posts, about 10.6 : 1 | TeamTavern posts 2019–2026 (rerun on production) | 2026-09-26 copy | Find a team |
| 2 | Role demand and supply table | TeamTavern posts | same | Pick a role |
| 3 | Share of group posts wanting scrims and tournaments, by game | TeamTavern posts | same | What a team is |
| 4 | Premier promoted over 40 teams into Challengers in 2024 | [Riot Competitive Ops](https://competitiveops.riotgames.com/en-US/VALORANT) | undated, 2026 content | To pro |
| 5 | Up to 1,280 teams per region per Challenger Circuit event; 30 Pro League teams per region | [EA ALGS Season Overview](https://algs.ea.com/en/season-overview) | Year 6 | Formats: Apex |
| 6 | 2 teams per region per year go from ESEA to ESL Challenger League | [HLTV](https://www.hltv.org/news/43293/esea-league-reworked-to-once-again-be-path-to-esl-challenger-league) | 2025-11-27 | Formats: CS2 |
| 7 | 12 Clash cups in 2026, 8-team brackets | [Riot Clash FAQ](https://support.riotgames.com/en-us/league-of-legends/events/clash-faq) | 2026-05-07 | Formats: League |
| 8 | NACE: 42 member schools (2018) to over 200 (late 2022) | [Wikipedia, citing NACE](https://en.wikipedia.org/wiki/National_Association_of_Collegiate_Esports) | read 2026-09-28 | Tryouts |

## Facts for the formats section, with sources

Confirmed on the publisher's own pages:

- **Premier:**
  - team creation, size 5–7 and the zone lock: [Riot, 2025-07-07](https://support.riotgames.com/en-us/valorant/gameplay/premier-team-creation-management)
  - eligibility (SMS, one ranked placement, good standing, the MMR rule): [Riot, 2026-03-30](https://support.riotgames.com/en-us/valorant/gameplay/premier-joining-leaving-teams)
  - divisions: [Riot, 2025-06-24](https://support.riotgames.com/en-us/valorant/gameplay/premier-divisions)
  - Contender and Invite, including the Immortal 3 peak: [Riot, 2025-12-22](https://support.riotgames.com/en-us/valorant/gameplay/premier-contender-invite-divisions)
- **Clash:**
  - level 30, ranked placements, SMS, tickets for every member, tiers I–IV, the 2026 calendar, and no Honor requirement: [Riot FAQ, 2026-05-07](https://support.riotgames.com/en-us/league-of-legends/events/clash-faq)
  - SMS rules: [Riot](https://support-leagueoflegends.riotgames.com/hc/en-us/articles/360000991627-Clash-SMS-Verification)
- **Dota:** Battle Cup free with Dota Plus: [dota2.com/plus](https://www.dota2.com/plus)
- **Overwatch:** OWCS 2026 FACEIT open qualifiers and promotion/relegation: [Blizzard, 2026-01-16](https://esports.overwatch.com/en-us/news/owcs-2026-season-competitive-details)
- **CS2:**
  - ESEA divisions and the path to ECL: [HLTV, 2025-11-27](https://www.hltv.org/news/43293/esea-league-reworked-to-once-again-be-path-to-esl-challenger-league)
  - Majors by VRS invite: [Valve rulebook](https://github.com/ValveSoftware/counter-strike_rules_and_regs/blob/main/major-supplemental-rulebook.md)
  - regional qualifiers cancelled: [HLTV, 2025-06-11](https://www.hltv.org/news/41930/valve-cancels-mrqs-ahead-of-budapest-major)
- **Apex:** Battlefy registration and the Circuit: [EA](https://algs.ea.com/en/season-overview) and [EA Split 2](https://algs.ea.com/en/news/algs-split-2-pro-league)
- **Siege:**
  - Ranked and Siege Cup paid since Siege X: [Ubisoft, 2025-06-10](https://news.ubisoft.com/en-us/article/5tIdWMRe5DKP4wZj321qCP/rainbow-six-siege-x-launches-today-free-access-now-available)
  - Challenger Series: [Ubisoft, 2026-01-20](https://www.ubisoft.com/en-us/esports/rainbow-six/siege/news-updates/2qbD0Cm5dSvxBfBxrIbW3i/r6se-challenger-series-2026)
  - EML Challenger Circuit €75k: [Ubisoft, 2026-03-02](https://www.ubisoft.com/en-us/esports/rainbow-six/siege/news-updates/7oZSh1xJJD83LoVnsdZ0rq/r6se-welcome-to-the-eml-challenger-circuit)
- **TF2:**
  - ETF2L divisions including Fresh: [ETF2L, 2026-05-04](https://etf2l.org/2026/05/04/announcing-6v6-season-52-summer-2026/)
  - Mann Up tickets and party of 6: [teamfortress.com](https://www.teamfortress.com/mvm/faq/)

**Not confirmed on an official source.** Leave these out, or check them in the client before stating them:

- Battle Cup ticket price, tiers and regional times. The only sources are from 2016 or a wiki.
- Clash ticket prices.
- The ESEA League Pass price.
- Siege Cup's schedule and account requirements, and Ranked's clearance level and 2FA.
- Apex substitutes and minimum ages.
- RGL season dates.
- Whether Premier is PC-only.

## Competitive gaps to exploit

1. **Specifics.** No ranking page names the in-game team formats, apart from one wiki that names Premier. None gives account requirements, and none gives the ladders.
2. **Data.** The role supply and demand, and the player-to-group ratio, exist nowhere else.
3. **Freshness and names.** Two ranking pages still say "CS:GO", and most show no date. Keep the Updated date visible and use current names: Counter-Strike 2, and "Overwatch", since the "2" was dropped in February 2026.
4. **Who it's for.** Every competitor writes for would-be pros. This guide is for the other 99%.

## Internal links

- **Links from this guide:**
  1. `/games/valorant`: "Valorant players and groups on TeamTavern"
  2. `/games/league-of-legends`: "League of Legends players and groups"
  3. `/games/overwatch`: "Overwatch players and groups"
  4. `/games/dota-2`: "Dota 2 players and groups"
  5. The CS2, Apex, Siege and TF2 feeds, one from each H3, with the same anchor pattern
  6. The sibling guides once they exist: "How to make an esports team", "Finding a Valorant Premier team", "Finding a team for League Clash", "Finding a team in Overwatch", "Writing an LFG post that gets answers"
- **Links to this guide:**
  1. The footer, through a "Guides" link to `/guides`. The footer holds only the site's own pages, and the guides index is one.
  2. The `/guides` index.
  3. Later, each game's "How <Game> LFG works on TeamTavern" section in [Feed.purs](../src/TeamTavern/Client/Pages/Feed.purs), linking its sibling format guide rather than this one.
- **Where it sits:** the hub of the "joining and running a team" guides. The format guides are its spokes.

## Trust signals and schema

- **No named author.** The site speaks as TeamTavern ("we") and names no person, so it has no Person schema. It uses Article with `author` and `publisher` as the TeamTavern Organization already declared on the home page, plus `datePublished` and `dateModified`.
- **BreadcrumbList:** Home > Guides > How to join an esports team.
- **Experience comes from the data.** The data section is first-hand evidence. Cite it as "TeamTavern's posts, 2019–2026". Write no "when we played Clash" stories unless the user supplies them.
- **Trust:** Every format fact links to the publisher's own page. Unconfirmed facts stay out. The guide links to TeamTavern where a reader would act, and nowhere else.

## Keeping it current

Change the Updated date only when a fact below changes, and review:

- **League:** each Clash season, since Riot's dates past October are marked "to be announced".
- **Valorant:** each Premier Stage and Riot's yearly esports announcement in December.
- **CS2:** each ESEA season.
- **Overwatch:** OWCS registration each January.
- **Apex:** each ALGS split.
- **Siege:** each Siege season.
- **TeamTavern's numbers:** once a quarter.

## Distribution

No social accounts, so none are planned.

- Ask for indexing in Search Console on publish.
- Link it from the footer and the guides index.
- Check in 4–6 weeks whether Search Console moves "how to join an esports team" from the home page to this URL.

An outside page already mentions TeamTavern for this topic: the [esports.net wiki](https://www.esports.net/wiki/guides/how-to-join-an-esports-team/) (2025-12-24) names it as a place to apply, beside Seek Team and Curry.gg. There's nothing to do about it; it's worth knowing it exists.

## Before publishing (site work)

The `/guides` section doesn't exist yet. It needs:

- a `State` case and path match in `Router.purs`
- an entry in the robots allowlist
- sitemap and llms.txt entries
- the guides index page
- a spec

The page itself can build on the `document` component that About and Terms use.
