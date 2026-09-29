# Content Brief: How to build a Marvel Rivals Championship team

The first game guide, and a spoke of [join-an-esports-team-brief.md](join-an-esports-team-brief.md). The join guide covers every game's team format in a paragraph. This guide takes one format, the Marvel Rivals Championship (MRC), and covers what a captain needs to recruit a team for it. It goes out after Marvel Rivals is on the site. The join and make guides then get a Marvel Rivals entry that links here.

Prepared 2026-09-29. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, Jun 2025 to Sep 2026
- the current results for the target queries
- NetEase's own patch notes, announcements and rules PDFs, with each one's date given where it's cited
- what the client shows, taken from a screenshot of the in-game rank restrictions panel, YouTube walkthroughs and Reddit reports, since nobody here has the game. Each is dated below, and none is cited in the guide.

TeamTavern has no Marvel Rivals posts yet, so this guide has no data of its own at launch. See "Adding TeamTavern's data" below.

## Template

**Recommended**: `how-to-guide`. The reader wants to know what to do before registration closes, in order. What sets the guide apart is that it gets the current season's rules right and cites them.

## Target keywords

Search Console has no volume for these queries, since the site has never had the game. The only Marvel Rivals queries that reached it went to the home page:

- "marvel rivals team finder": 21 impressions at position 45.8
- "marvel rivals group finder": 11 at 50.8
- "marvel rivals duo finder": 4 at 41.3

Those queries ask for listings, and `/games/marvel-rivals` answers them. Overwatch is the closest game already on the site, and it shows the same split. Its feed ranks for listing queries like "overwatch team finder" (1,193 at 7.3), "overwatch esports teams recruiting" (402 at 9.6) and "overwatch teams to join" (266 at 7.3). No guide takes those from it. This guide targets what a listing can't answer:

- **Primary:** marvel rivals championship team, and how to join the Marvel Rivals Championship
- **Secondary:**
  - marvel rivals faction, how to make a faction, and how to join a faction
  - marvel rivals championship rank requirement
  - marvel rivals party restrictions ranked, marvel rivals can't queue 5 stack, and "team member rank gap is significant", the game's own error text
  - marvel rivals crossplay ranked
  - how to qualify for marvel rivals ignite
- **Leave to the feed:** "marvel rivals team finder", "lfg", "duo finder", "group finder", "teams recruiting", "teams to join"
- **Leave out:**
  - hero tier lists, team-up abilities and counters. That's gameplay, which is outside what the site is about.
  - pro team rosters and Ignite results. That's esports news.
- **Questions:**
  - What rank do you need for the Marvel Rivals Championship?
  - How many players are in a Faction?
  - Can PC and console players play Competitive together?
  - Why can't I queue Competitive as a party of five?
  - Can console players reach Ignite?

Measure after launch instead of guessing volumes: see Distribution.

## Search intent

The intent is informational, with a deadline behind it. The reader has seen the Tournament tab or a friend's Faction and wants to enter this season.

The results fall into three kinds, and none of them helps that reader:

- **Sign-up funnels.** curry.gg, teamplay.gg, tracker.gg's LFG and theteamsfinder rank for "team finder" and "how to find a team". None of them mentions Factions, the rank bar or the platform split. marvelrivals.gg's matchmaking page (updated Mar 2025) is mostly a plug for curry.gg.
- **Rules copied from somewhere else.** The fandom wiki, Tribality, Escapist (Jan 2025) and several rank guides give party restrictions that contradict each other. Few are dated, and none cites the Season 5 patch notes that set the current rules. One 2026 boosting blog allows 6-stacks at every rank and trios in Grandmaster, which the official notes contradict.
- **Explainers of what a Faction is.** Deltia's, FandomWire, Sportskeeda and the rules PDFs cover what a Faction is. None covers how to recruit one. Every "Ignite open qualifier" result describes 2025 and implies an open qualifier that no longer exists.

Say in the first paragraph who the guide is for: a player putting together a Faction for this season's Championship, or climbing to its rank bar with a stack.

**The game has a recruiting tool of its own, and the guide says so.** A captain creates a Faction and sets it to "Free to Join" or "Apply to Join", with a rank requirement. Players can browse and filter Factions and see each one's language, server region, rank requirement, roles and member count. The guide shows how to use that, then says what it can't do: a player can't list themselves, and a captain can't search for players. That's what an LFG post does, on TeamTavern and elsewhere. Players post there with what the in-game list doesn't show: their hours, voice, and whatever they write about themselves.

**Where players recruit for Factions today:**

- [r/MarvelRivalsLFG](https://www.reddit.com/r/MarvelRivalsLFG/), about 5.5k members. Its post template is Region / Platform / Rank / Role(s) / Mode, which is the seed's field set. Season 10 MRC posts are running there now ("NEED 1 OR 2 ppl for mrc PS5 NA", 2026-09-19).
- r/marvelrivals ("Currently Organizing a Team For the Season 10 MRC… (Platinum 3 or Higher)", 2026-09-15).
- The official Discord's LFG channels, by region and platform, and by one report a Faction recruitment section.

The guide mentions the in-game list and the official Discord. It doesn't link the subreddit, in keeping with the join guide's rule of linking only official sources and TeamTavern.

## Content parameters

- **Word count:** 1,700–2,100. The join guide's Marvel Rivals section stays at 3–4 sentences and links here.
- **Reading level:** plain and direct, like the other two guides. It says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the other guides, Markdown in `Server/Guide/`.
- **H2 sections:** 6, plus an FAQ.
- **Visuals:**
  - the game's cover, as in the join guide's per-game sections
  - one table: what a Faction needs. The specs' `getByRole("table")` is strict, so there's only one table.
  - the party rules as a short list, not a second table
  - no chart at launch
- **FAQ:** 5 questions.
- **Naming:** use the game's class names alongside the site's the first time they appear: "Tank (Vanguard), DPS (Duelist) and Support (Strategist)". After that, use Tank, DPS and Support as the seed does.

## Recommended title

**Marvel Rivals Championship: how to build a Faction and qualify**

Alternatives:

1. How to join the Marvel Rivals Championship: rank, roster and platform rules
2. Building a Marvel Rivals Championship team, from Platinum 3 to Ignite

H1: "How to build a Marvel Rivals Championship team".

Slug: `marvel-rivals-championship-team`. It's permanent, so it names the format, not "team finder" and not a season.

## Meta description

Every member at Platinum 3, one platform, a roster that locks at sign-up: how to recruit a Faction for the Marvel Rivals Championship, who you can queue ranked with, and the way to Ignite.

## TL;DR draft

> The Marvel Rivals Championship is the game's own tournament for teams, run every season from the Tournament tab. Your team is a Faction of six to twelve players, all on the same platform. Each one must reach Platinum 3 in that season's Competitive before the captain registers. The roster locks once registration succeeds, so register seven or eight, not six. While you climb, the game lets players from Gold to Celestial party only within three divisions of each other, and from Celestial up only in pairs. On PC, the Championship is the only open way into Ignite, the pro circuit. Console Factions play their own Championship, which is as far as console goes.

## Information gain

- **[UNIQUE INSIGHT] The recruiting rules follow from the tournament's rules.** None of the competitors draws these out, and each follows from a confirmed rule:
  - A Faction needs at least 6 members and holds up to 12. The roster locks from registration until the Faction is out, and "if a registered Faction has more than 6 members, they are free to decide the lineup before each match". So register 7–8, since nobody can be added after.
  - Players on different platforms can't share a Faction, and a Faction's platform is its creator's. Rank is kept per platform, and separately on PlayStation and Xbox too. So recruit on your platform, and check each player's rank on the platform they'll play on.
  - Every member needs Platinum 3 in the current season, and the season's rank resets. So recruit players who can get to Platinum 3 early in the season, not ones who got there last season. Season 10's open qualifier came four weeks after the season started.
  - The open qualifier is up to 10 best-of-1 matches over one weekend, in the evening in the region's time. So recruit for those two evenings first.
  - There's no role queue, so a Faction is the one way to be sure of a composition. Recruit for the roles you're missing.
- **[UNIQUE INSIGHT] Rank spread decides who can climb together.** The in-game panel's rules:
  - Gold to Celestial can party only within three divisions.
  - From Celestial up, only solo or duo.
  - Bronze to Grandmaster, any party size except five.

  So a Faction whose members are spread from Gold to Diamond can't climb together, and a Faction at Celestial and up can't queue together at all: it practises in custom games and scrims. No competing page links the party rules to building a team.
- **[UNIQUE INSIGHT] The two paths.** PC Factions go from the Championship to Champion Points to Ignite. Console Factions top out at their own Championship. A console player who wants the pro path has to play Competitive on PC, where rank starts over. Say this plainly: no competitor does.
- **[FRESHNESS] Current, dated, cited.** Every competing rules page is stale or has no date. This guide dates its facts and links the patch notes and rules. Keeping it that way each season is the edge, so see "Keeping it current".

## Content outline

### Introduction (~130 words)

- **Hook:** Nearly every Marvel Rivals "team finder" page is a sign-up form. None of them says your whole team has to be at Platinum 3 on the same platform before it can enter the game's own tournament.
- **Who it's for:** a player putting together a Faction for the Championship this season, or a stack to climb to its rank bar with.
- **Promise:** what the Championship is, what a Faction needs, who you can queue ranked with while you climb, how to recruit, and the way to Ignite.
- The TL;DR box goes here.

### H2: What the Marvel Rivals Championship is

- **Answer first:** The Championship is Marvel Rivals' own tournament for teams, run once a season from the Tournament tab. Teams are called Factions, and they play in their region against Factions on the same platform.
- Cover:
  - **Seven events:** China PC, plus a PC event and a Console event for each of Americas, EMEA and Pacific.
  - **The stages:**
    - a sign-up stage of about two weeks
    - an open qualifier: up to 10 best-of-1 matches over one weekend, in the evening in the region's time
    - the top 128 go straight to double elimination, and places 129–512 play a closed qualifier the next weekend
    - double elimination for 256 Factions over about two weeks, played best-of-3, then best-of-5, then best-of-7
  - Don't describe how the open qualifier pairs teams or scores them. Season 7's rules give 3 points a win, and Liquipedia calls Seasons 9 and 10 a Swiss system.
  - **Rewards:**
    - outside China, in Season 7, $7,000, $3,500, $2,500 and $1,500 for first to fourth in each region
    - Units and cosmetics for playing at least 6 open-qualifier matches
    - Champion Points on PC (see the Ignite H2)
  - **When it runs:** once a season. In Season 7, sign-up ran 28 Mar to 11 Apr 2026, and the open qualifier followed that weekend. Say that each season's dates are on the Tournament tab. Name a season's dates only from NetEase's own rules; Liquipedia's are for the brief.
- Game cover beside the first paragraph, as in the join guide.

### H2: What your Faction needs to register

- **Answer first:** at least six players on one platform, every one of them at Platinum 3 in this season's Competitive. The captain or coach registers in the Tournament tab, and registration succeeds once every member confirms.
- **Table: what a Faction needs.** Columns: Requirement / The rule / What it means for recruiting. Rows:
  - **Members:** at least 6, and a Faction holds up to 12. The Faction picks its lineup before each match. *Register 7–8.*
  - **Rank:** Platinum 3 in the current season's Competitive, for every member. *Rank resets each season, so check it this season.*
  - **Platform:** players on different platforms can't share a Faction, and PC and console play separate events. *Recruit on your platform.*
  - **Registration:** in the Tournament tab only. The captain or coach registers, and every member confirms. The captain can add or remove members until everyone has confirmed, and registration fails if the Faction drops below 6 or anyone hasn't confirmed by the deadline. *Everyone has to be online during the window.*
  - **Roster lock:** once registration succeeds, or the deadline passes, nobody can be added or removed until the Faction is out. A Faction can cancel its registration twice before the deadline. *Add the subs first.*
  - **Places:** a cap per region, 2,500 in Season 7. *Register early in the window.*
- Under the table, two sentences:
  - The latest rules NetEase publishes are Season 7's (v1.7), so check the Tournament tab for this season's.
  - A player on an Ignite team's roster can't play in the Championship.
- Leave age out of the table: see "Not confirmed".

### H2: Climbing to Platinum 3: who you can queue with

- **Answer first:** In Competitive, players from Gold to Celestial can party only within three divisions of each other. From Bronze to Grandmaster any party size is allowed except five, and from Celestial up only solo or duo.
- Cover, as a list:
  - Competitive needs account level 15.
  - The three-division limit starts at Gold. Eternity and One Above All players can team with a Celestial II player within 200 points.
  - When a party's ranks are too far apart, the game refuses the queue with "Team member RANK gap is significant." This is the query people search, so quote it.
  - While anyone in the party is in placements, the party is capped at 3, and the highest and lowest members must be less than 3 divisions apart.
  - PC and console players can't queue Competitive together. PlayStation (PS5 and PS4) and Xbox players can. Each platform keeps its own rank, so the same player can be Grandmaster on PlayStation and Gold on Xbox.
  - The reset: 10 placement matches. The estimated starting rank is last season's final rank less 3 divisions, capped at Grandmaster II, and since Season 7 placements move it up to 100 points either way by your own play.
- **Close with what this means for a Faction:** members within three divisions of each other can climb together, up to a ranked six-stack below Celestial. Otherwise, climb in smaller parties of close ranks. From Celestial up the Faction can't queue together at all, and practises in custom games and scrims against other Factions.

### H2: Recruiting your Faction

- **Answer first:** Create the Faction in the game, then recruit for the qualifier weekend and the roles you're missing, on your platform and your region's servers. Recruit seven or eight so a missed evening doesn't end your run.
- Cover:
  - **Create the Faction:** Tournament → Faction → Create Faction. It takes a name and tag, a primary language, a region, a rank requirement for applicants, and an admission rule: "Free to Join" or "Apply to Join". The platform is the creator's. The creator is the captain. For the Championship, set the rank requirement at Platinum 3, not "None".
  - **The in-game list:** other players browse and filter Factions and apply. It lists your Faction, but it doesn't bring you players: a player can't list themselves there, and a captain can't search for players. So also post where players look.
  - **Fix the evenings first:** the open qualifier's two evenings, then a practice night each week before it.
  - **Roles:** there's no role queue, so the Faction decides its composition. Name the roles you're missing in your group post, not all three.
  - **Server:** agree on one region's servers. Oregon, Dallas and Northern Virginia for North America; Frankfurt and Warsaw for Europe; Dammam; São Paulo; Tokyo, Singapore and Sydney. The seed's Server field is this list.
  - **Tryout:** a few ranked games together if your ranks allow it, or a custom game against another Faction.
  - One line linking the make guide for tryouts and keeping a roster together, instead of repeating it: "How to make an esports team".
- **The TeamTavern step:** put up a group post with your rank range, the roles you're missing, your server and platform, and "Marvel Rivals Championship" ticked under Looking for. Then message the player posts that fit.

### H2: From the Championship to Ignite

- **Answer first:** On PC, the Championship is the only open way into Ignite, the game's pro circuit. There's no separate open qualifier in 2026. Console has no path beyond its own Championship.
- Cover:
  - Championship results on PC earn Champion Points. By the Season 10 notes, Season 11's Championship is the last that counts for 2026. Its top Factions and the Champion Points leaders go to the 2026 Annual Qualifier, which plays for places in the 2027 Pro Circuit.
  - A Faction that qualifies must keep at least 4 of its Championship players, and every member must meet the age of majority when the roster goes in.
  - Ignite's own rules: PC only; 6 starters, up to 2 subs and up to 2 managers or coaches; proof of residency, with at most 2 players from outside the region; 12 Partner teams invited directly.
- **Keep it short.** Esports news goes out of date in a month, and this H2 needs rewriting when the 2027 season is announced.

### H2: Find your Faction

- Two short paragraphs and the link: post as a player with your rank, role, server and platform, and tick "Marvel Rivals Championship". Or post as a group with the roles you're missing. Link `/games/marvel-rivals` with the anchor "Marvel Rivals players and groups on TeamTavern", as the join guide does for each game.
- Mention the official Marvel Rivals Discord's LFG channels in one sentence as another place players look. It's official, so it's a fair mention and not a competitor plug. Link `https://discord.gg/marvelrivals`.

### FAQ (5)

1. **What rank do you need for the Marvel Rivals Championship?** Platinum 3 in the current season's Competitive, for every member, on the platform the Faction plays on.
2. **How many players are in a Faction?** At least six to register and up to twelve in all. The Faction picks its six before each match.
3. **Can PC and console players play Competitive together?** No. PC and console are matched separately, and the Championship runs separate PC and Console events. PlayStation and Xbox players can queue together, though each platform keeps its own rank.
4. **Why can't I queue Competitive as a party of five?** Five is the one party size Competitive refuses, from Bronze to Grandmaster. Queue as four, or add a sixth. From Celestial up, it's solo or duo only.
5. **Can console players reach Ignite?** Not from console: Ignite is PC only in 2026. The Console Championship is the top of the ladder there.

### Conclusion (~100 words)

- Takeaways: rank up early in the season, recruit 7–8 on one platform, register the subs, climb with players near your rank.
- **Call to action:** "Marvel Rivals players and groups on TeamTavern".

## Facts, with sources

Checked 2026-09-29.

**NetEase's own pages. Cite these:**

- **Championship rules**, from the [MRC S7 Rules v1.7](https://www.marvelrivals.com/Marvel_Rivals_Championship_S7_Tournament_Rules_V1.7_EN.pdf), the latest NetEase publishes. The S8, S9 and S10 rules aren't online. The text was extracted and read in full.
  - "a quarterly event that follows the game's seasons", whose rules and requirements may change each season
  - S7 ran 28 Mar to 10 May 2026. Sign-up ran 28 Mar 08:00 to 11 Apr 18:00, in each region's time: UTC−5 for Americas, UTC+2 for EMEA, UTC+9 for Pacific.
  - "Factions must be formed through the in-game 'Faction' system." Registration is in the Tournament page only, by the captain or coach, and succeeds "once all Faction members confirm"; it fails if anyone is unconfirmed at the deadline or the Faction drops below 6. "During the sign-up stage confirmation period, the Faction captain can freely add or remove members."
  - "Once registration is successful or deadline of the sign-up stage arrives, Factions cannot add, remove, or change members." The lock holds "until the Faction is eliminated or achieves a final ranking". Two cancellations are allowed before the deadline. "If a registered Faction has more than 6 members, they are free to decide the lineup before each match. Factions are advised to include enough substitutes in case of an emergency."
  - "a minimum rank of Platinum 3 for registration… all Faction members must have reached the required rank in the relevant season's competitive mode"
  - "Faction registration limit for this Championship: 2,500 Factions/Region"
  - "Players from different platforms cannot join the same Faction." "The platform of an account is determined by the device used for registering." PC only in China; PC and console in Americas, EMEA and Pacific, each event independent.
  - "Players who are already registered on the member lists of any teams of Marvel Rivals Ignite Series 2026… are prohibited from participating in the MRC in any form." A Faction that qualifies for Ignite through the MRC must have all members of adult age when it submits its roster.
  - the open qualifier: up to 10 best-of-1 matches, 19:00–23:00 regional time, 3 points a win; top 128 to double elimination, 129–512 to the closed qualifier; double elimination for 256 Factions, best-of-3, then best-of-5, then best-of-7
  - prizes outside China: $7,000 / $3,500 / $2,500 / $1,500; Units and cosmetics for 6+ open-qualifier matches
- **The S7 announcement**: [Season 7 Marvel Rivals Championship: The hunt is on, 2026-03-26](https://www.marvelrivalsesports.com/20260326/42828_1293068.html): "Each Faction must contain at least 6 players", "Players must register through the 'Tournament' page in the game."
- **What changed since the [S0 rules](https://www.marvelrivals.com/MarvelRivalsChampionship.pdf) (Dec 2024):** the rank bar went from Silver 3 to Platinum 3, the cap from 1,500 to 2,500, first prize from $6,000 to $7,000, and the regions from NA/EU/Asia to Americas/EMEA/Pacific. Useful for knowing which competitors are out of date. Not for the guide.
- **Platinum 3 still applies:** the S6 notes say it's "still the same". A Season 8 video (2026-06-19) shows it still in place, and nothing later changes it.
- **Party rules:**
  - [S5 patch notes, 12 Nov 2025](https://www.marvelrivals.com/gameupdate/20251112/41548_1270590.html): Bronze to Grandmaster, any party size except 5; Celestial and above, solo or duo; a party with anyone in placements has at most 3 players, spread less than 3 divisions; 10 placement matches; starting rank is last season's final rank less 3 divisions, capped at Grandmaster II
  - [S2 patch notes, 8 Apr 2025](https://www.marvelrivals.com/gameupdate/20250401/41548_1223996.html): Competitive needs account level 15; any party with an Eternity or One Above All player is solo or duo
  - S6 to S10 notes: no change to party rules
- **Placements by your own play**, up to 100 points either way: [S7 notes, 18 Mar 2026](https://www.marvelrivals.com/gameupdate/20260318/41548_1291772.html)
- **Rank per platform**: [Cross-Progression Q&A](https://www.marvelrivals.com/announcements/20250924/40955_1261246.html): "The rank system in Marvel Rivals is designed separately for each platform (PC, PlayStation®, Xbox)."
- **PC and console not matched in Competitive:** [closed-beta FAQ, 19 Jul 2024](https://www.marvelrivals.com/news/official/20240719/40185_1168280.html). It's old, but the per-platform rank and the separate Championship events agree with it.
- **Servers and multi-server selection:** [official game guide](https://www.marvelrivals.com/m/guide/), undated. Oregon, Dallas, N. Virginia / Frankfurt, Warsaw / Dammam / São Paulo / Tokyo, Singapore, Sydney. This matches the seed's Server options.
- **Current season:** Season 10 launched 11 Sep 2026: [S10 notes, 9 Sep 2026](https://www.marvelrivals.com/gameupdate/20260909/41548_1313441.html). The same notes say S11's Championship is the "final stop for 2026 Champion Points", and that its top Factions and the Champion Points leaders go to the Ignite Series 2026 Annual Qualifier, with select pro-circuit teams, for 2027 Pro Circuit spots.
- **Ignite 2026**, from the [Preseason & Stage 1 rules v1.0](https://www.marvelrivals.com/Marvel_Rivals_Ignite_2026_Rules_Preseason_&_Stage1_2026.3.13_V1.0.pdf): PC only; 6 starters, up to 2 subs, up to 2 managers or coaches; age 16+ (18+ and Chinese citizenship in China), a guardian's consent for minors; proof of residency, at most 2 players from outside the region; 12 Partner teams invited; a qualified roster keeps at least 4 of its MRC players. The [Ignite Stage 2 Rules v1.4](https://www.marvelrivals.com/Marvel_Rivals_Ignite_Rules_Stage_2_V1.4.pdf) PDF on the same site is the **2025** rulebook. Don't cite it.
- **PS4:** supported since 12 Sep 2025, and still in the [S9.5 notes, 7 Aug 2026](https://www.marvelrivals.com/gameupdate/20260805/41548_1310120.html). The seed's PlayStation option covers it.
- **Official Discord:** [the server listing](https://discord.com/servers/marvel-rivals-1193841000108531764), about 4.49M members, "Find friends to play with in our LFG channels!"

**From the client, through screenshots, videos and players' reports.** Enough to state as fact in the guide, never cited:

- **The in-game "RANK RESTRICTIONS" panel**, a screenshot posted 2026-01-11 ([image](https://i.redd.it/vhp5h3nvnncg1.png), [thread](https://old.reddit.com/r/marvelrivals/comments/1q9qxnw/)):
  1. "Gold to Celestial tiers can team within a maximum range of three divisions."
  2. "Eternity and One Above All players can team with Celestial II players within 200 points…"
  3. "Players from Bronze to Grandmaster can form teams of any size except five players."
  4. "Above Celestial, players may only join Competitive as solos or duos."

  Players reported hitting the same limit in April, June and September 2026. The panel starts the rule at Gold, so Bronze and Silver players aren't held to it outside placements.
- **Error texts:**
  - "2132: Team member RANK gap is significant." ([screenshot, 2025-07-08](https://i.redd.it/yrsg59xk6kbf1.jpeg)). No 2026 screenshot, so quote it without the code number.
  - "Has placement match player, rank gap too great", reported word for word on 2026-05-28 and 2026-09-17.
- **Consoles share one Competitive pool:** PS5, PS4 and Xbox, from several players in Sep 2025, May 2026 and Sep 2026. Rank is still per console: "GM2 last season on my ps5 and now on my Xbox I'm in gold" (2026-09-12).
- **A Faction holds up to 12 members:** captains in Dec 2025 ("The faction is limited to 12 people") and Feb 2026 ("11 out of 12"), and Sportskeeda (Dec 2024).
- **Creating a Faction:** Tournament → Faction → Create Faction, with a name, abbreviation, primary language, rank requirement for applicants ("None" allowed), admission rule "Free to Join" or "Apply to Join", region, platform and emblem. The creator becomes captain. From walkthroughs dated 2024-12-06, 2025-02-09 and 2026-01-26. The platform being the creator's, and fixed, is from one 2025 video, and it agrees with the rulebook's "determined by the device used for registering".
- **Browsing Factions:** a searchable, filterable list that shows "language, server region, the rank requirements, roles, and member count"; "apply to join"; "Some will require approval" (JustBOZ, 2026-01-26).
- **Registering:** members confirm, or tap "follow" the captain to accept automatically (2025-01-17 video; Reddit 2026-01 and 2026-02). The in-game error for a short or under-ranked Faction: "requires at least 6 members, all of whom must meet this seasons competitive rank" (2026-01-24).
- **Season 10 Championship dates**, from [Liquipedia](https://liquipedia.net/marvelrivals/Marvel_Rivals_Championship/Season_10/Americas/PC): open qualifier 10–11 Oct 2026, closed qualifier 17–18 Oct, double elimination 24 Oct to 8 Nov. S9 was 8–9 Aug, 15–16 Aug and 22 Aug to 6 Sep. Liquipedia's Ignite page puts S11's Championship in January 2027.
- **Season 10 ends** 13 Nov 2026 by the fandom wiki, with Season 10.5 from 9 Oct.

**Not confirmed, or sources disagree.** The guide handles each like this:

- **Age.** The S7 rulebook sets "no age limit for players registering for other regions" than China. The S7 announcement says players "must be at least 16 years old", with a guardian's consent for minors. The guide gives no age for the Championship. TeamTavern's own minimum is 16 anyway. For Ignite, it gives the adult-age rule for qualifying Factions.
- **The cap.** The S7 rulebook says 2,500 per region; Liquipedia says 2,000 for the S10 pages. The guide says "a cap per region, 2,500 in Season 7".
- **The open qualifier's format.** The S7 rulebook gives points, and Liquipedia calls S9 and S10 Swiss. The guide says only "up to 10 best-of-1 matches".
- **Party server selection.** One player (2025-10) says the leader's choice shows to the party, and a 2025 party screenshot shows one selector. The guide says to agree on servers and doesn't explain the mechanism.
- **Exact button labels in 2026** ("Free to Join", "Apply to Join"). Walkthroughs from 2024–25 and a 2026 player ("some are open join and some are apply to join") agree. Use the labels, and review them on a 2026 video before publishing if one turns up.
- **The official Discord's Faction recruitment section**: one player's report (2026-07-20). The guide mentions only the LFG channels.
- **A Discord link as a Championship requirement**, and "within one rank of each other": one player on 2026-09-21. Leave both out.
- **"Marvel Rivals Clash"**: NetEase ran one cross-platform bracket with no rank bar, on 21–22 Feb 2026 ([S6.5 notes](https://www.marvelrivals.com/gameupdate/20260211/41548_1286781.html)). It hasn't recurred, and later "Clash" events on Liquipedia are third-party. Leave it out.
- **The Annual Qualifier's size, dates and platform.** Nothing published. It's PC by inference from Ignite being PC-only. The guide says only what the S10 notes say.
- **The [Team Certification Program](https://www.marvelrivalsesports.com/20260414/42828_1295963.html)** (2026-04-14): an official verified badge and a protected team name, with applications from the 1st to the 20th of each month. It's for teams heading for esports, not for recruiting. Leave it out, or give it one sentence in the Ignite H2 if the draft has room.

## Adding TeamTavern's data

At launch the guide has no chart. Once Marvel Rivals has 50 or more group posts that answer the role question, the bar the join guide uses for Siege, add the join guide's role chart for Tank, DPS and Support, groups asking against players offering, in the game's order. Add a line giving the share of group posts that tick "Marvel Rivals Championship". That's a meaningful rewrite: change `updated`.

## Internal links

- **Links from this guide:**
  1. `/games/marvel-rivals`, twice: in the recruiting H2 and in "Find your Faction". Anchor: "Marvel Rivals players and groups on TeamTavern".
  2. The make guide, from the recruiting H2. Anchor: "How to make an esports team".
  3. The join guide, from the introduction. Anchor: "every game's team format".
- **Links to this guide:**
  1. The join guide: a "Marvel Rivals: the Championship" H3 under "Your game's team format", 3–4 sentences and a link. It also gets a row in its format table: MRC, 6–12 players | Platinum 3 this season, one platform | Once a season. Rivals is also a busy-feed candidate for its closing line once posts arrive.
  2. The make guide: a row in its format table (captain or coach registers in the Tournament tab | the roster locks at registration until the Faction is out).
  3. The `/guides` index.
  4. The Marvel Rivals feed. It doesn't link any guide yet: "How <Game> LFG works on TeamTavern" in `Client/Pages/Feed.purs` is the same for every game. Proposed as a separate change: a guide in `Guides.purs` names the game handles it covers, and a feed's about section links the guides that name its game. The join and make guides would name none, or every game. It pays off as more game guides arrive.
- Editing the join and make guides adds a fact, so their `updated` changes too.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, and logo-512.png as `image`, as the other guides have. BreadcrumbList Home > Guides > How to build a Marvel Rivals Championship team. `datePublished` and `dateModified`. No Person.
- Every rule the guide cites links to NetEase's own page, on the words that name it, once per source per section. What comes from the client (the rank panel, the Faction screen, the error text) is stated as what the game shows, without a link. Nothing from fan sites.
- Where a rule is from an older season's PDF, the sentence says which season, e.g. "Season 7's rules".
- No "when we played" stories.

## Keeping it current

A Marvel Rivals season lasts about two months. Review with each season's launch notes and each new Championship:

- **Party rules and placements:** the season's patch notes.
- **The Championship:** the Tournament tab, and any new rules PDF.
- **Ignite:** the 2026 Annual Qualifier after S11, and the 2027 circuit announcement. The Ignite H2 will need a rewrite for 2027, probably in early 2027.

Change `updated` only when a fact changes.

## Implementation

- `Server/Guide/marvel-rivals-championship-team.md`, imported as text in `Guides.js`, and an entry in `Guides.purs`: slug, heading, title, description, dates.
- The Marvel Rivals H3 and table row in `join-an-esports-team.md`, and the row in `make-an-esports-team.md`.
- The cover is already at `/images/games/400/marvel-rivals.webp`.
- The specs that cover guides must still pass: one table per guide, heading ids for fragment links.

## Timing

1. Deploy the `marvel-rivals` branch first, with `Migrations/2026-09-29-marvel-rivals-username.sql` applied to development and production. The guide links a feed that must exist.
2. **Season 10's Championship is already under way.** Its open qualifier is on 10–11 Oct 2026 by Liquipedia, so sign-up is open now or about to close. A guide published in the first week of October could still reach captains filling their last places. A guide published later loses nothing: the rules are the same next season.
3. **Season 11's Championship is the main target:** January 2027 by Liquipedia, and the last that counts for 2026 Champion Points. Publish by mid-November, when Season 11 starts and players begin climbing to Platinum 3.

## Distribution

The same plan as the other guides:

- request indexing in Search Console on publish
- link it from the join guide, the make guide and the guides index
- after 4–6 weeks, check which Marvel Rivals queries reach the guide and which reach the feed. If a listing query ("teams recruiting", "team finder") lands on the guide, the guide is competing with the feed, so adjust its title.
