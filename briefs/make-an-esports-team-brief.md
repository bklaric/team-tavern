# Content Brief: How to make an esports team

The captain's side of [join-an-esports-team-brief.md](join-an-esports-team-brief.md). The two guides launch together and link to each other.

The join guide owns the format facts: what each tournament asks of a player's account, its schedule and its path to pro. This guide links there and doesn't repeat them. What this guide owns is what only a captain does: setting up the team, recruiting, running tryouts, finding scrims, setting it up in a format, and keeping the roster together.

Prepared 2026-09-28. Evidence comes from:

- Search Console for sc-domain:teamtavern.net, Jun 2025 to Sep 2026
- the current results for the target queries
- the organisers' own support pages and rulebooks, with each one's date given where it's cited
- TeamTavern's own posts in the development database, a copy of production taken 2026-09-26

## Template

**Recommended**: `how-to-guide`. The pages that rank are step lists, and the reader wants steps. What sets this one apart is that its steps carry each format's real rules.

## Target keywords

These are Search Console impressions for the home page, which ranks without answering the question. The whole family comes to about 800 impressions, much less than the join guide's. It's worth writing mostly because the two guides serve each other.

- **Primary:** how to make an esports team. 104 impressions at position 24.4. Its spellings add ~190 more: "how to make a esports team" 86, "make a esports team" 87 at position 11.7, "create esports team" 10.
- **Secondary:**
  - how to create an esports team: 112 at position 33.2, plus 97 for "a esports team"
  - how to start an esports team: 76 at position 50.4, plus 62 for "a esports team"
  - build your own esports team: 27
  - player-owned esports team: 23 at position 6.7
- **Worth noting:** long, AI-style questions already bring TeamTavern up in the top 3:
  - "I want to build an esports team from the ground up. What's the best platform to help me recruit and manage players?": 85 impressions at position 3.0
  - "how to recruit and build a professional esports roster": 33 at position 3.0

  A guide that answers the recruiting half of those questions gives assistants and AI Overviews a passage to quote.
- **Leave out:**
  - "how to create an esports organization", and the branding, sponsor and LLC side of the query. Senet, GGCircuit and CorrosionHour cover it, and it's not TeamTavern's reader.
  - "lol team builder": a champion-select tool, a different intent.
- **Questions:**
  - How many players do you need to start an esports team?
  - How much does it cost to run an amateur team?
  - Do you need a high rank to start a team?
  - Should an amateur team have a written agreement?
  - Where do I find players for my team?
  - How often should a team practise?

## Search intent

The intent is informational. The results mix two readers:

- someone founding an esports organisation (logo, sponsors, website)
- a school or club starting a programme (Edutopia, British Esports)

The reader TeamTavern serves is a third one, and only [Tournova](https://tournova.games/blog/how-to-build-an-esports-team-from-scratch-without-burning-out-in-six-months/) (2026-08-30) and PaxJax's recruiting post speak to them. That reader is a player who wants their own stack for Premier, Clash, ESEA or Siege Cup, is short of two or three people, and doesn't know how the format treats a roster. Say who the guide is for in the first paragraph, and send organisation-builders elsewhere in one sentence.

## Content parameters

- **Word count:** 2,000–2,500. Competitors run 1,100–5,500. The roster rules table carries the facts, and the prose stays short.
- **Reading level:** plain and direct, like About. The copy says "we" or uses the passive, never "I".
- **Format:** the same `document` page as the join guide.
- **H2 sections:** 7, plus an FAQ.
- **Visuals:**
  - one table of roster rules, the section competitors don't have
  - one small chart from TeamTavern's data
  - no stock photos
- **FAQ:** 5 questions.

## Recommended title

**How to make an esports team: a captain's guide for Valorant, League, CS2 and more**

Alternatives:

1. How to start an esports team with your friends (or without them)
2. How to build an esports team: recruiting, tryouts and roster rules

H1: "How to make an esports team".

## Meta description

Starting an amateur team? How to pick your format, recruit for the roles you're missing, run tryouts, find scrims, and the roster rules each game's tournament enforces.

## TL;DR draft

> An amateur esports team starts with a decision, not a logo. Pick the format, the night you play and a rank band about a tier and a half wide. Then recruit for the roles you're missing, by name. Players outnumber groups ten to one on TeamTavern, so the common roles fill fast and the scarce ones don't. Try people out in a real scrim, then set the team up in your format, and learn its roster rules before you need them. Some of them punish a late change: removing a Premier teammate after your first match can mean you can't add them back. Write down who decides what and how people leave. Low commitment and doubts about the team's future are what predict players wanting to leave.

## Information gain

- **[ORIGINAL DATA] Supply and demand, seen by a captain.**
  - 26,831 player posts against 2,538 group posts, so about ten players per group.
  - The roles groups ask for against the roles players offer, both counted only among posts that answered the role field:
    - CS2 support: 68% against 38%
    - Dota 2 mid: 67% against 40%
    - League top: 58% against 36%
    - League jungle: 57% against 38%
    - Overwatch: nearly level
  - The captain's reading: expect the common roles to fill in days and the scarce ones to take longer. Say what you offer a scarce player: a regular slot, their preferred role, or someone else calling.
- **[ORIGINAL DATA] What group posts leave out.** Counted on TeamTavern's group posts, Valorant and CS2 excluded (see caveats):
  - **30% tick every role**, which tells a player nothing about the gap. Only half name one or two.
  - **One in five** doesn't say the team uses voice (79% do).
  - **Almost half** give no hours (53% do).
  - Rank ranges have a median width of **5 divisions in Valorant and 6 in League**, about a tier and a half. That's a useful default for a captain setting one.
- **Caveats for the data:**
  - Posts from the old site came through the relaunch import's mapping.
  - Valorant roles are unusable, since only "entry fragger" mapped (to Duelist).
  - CS2's "Anchor" role is new in the seed, so no imported post has it, and "every role" can't be counted there.
  - The imported group posts carry no group size.
  - Rerun on production before publishing, and put the date beside the numbers.
- **[UNIQUE INSIGHT] The roster rules that bite.** No competitor covers the roster mechanics of any format. The ones that change what a captain does:
  - **Premier:** after the team's first match, nobody can join whose MMR would move the team's division, and Riot warns that a removed player may not be re-addable.
  - **ESEA:** in the higher divisions, 2 of last season's players must play 3 of the first 4 matches to keep the slot.
  - **ALGS:** points belong to the players, not the team name.
  - **Siege Cup:** a registered team can't be edited, so a no-show means playing a player short.
  - **ETF2L:** a player can change team only twice per competition.
- **[UNIQUE INSIGHT] The captain and the owner are separate jobs.** Premier makes that split itself: the owner sets up the team, and the captain invites and removes. Suggest the same split for any team: someone runs the roster, and someone books the scrims.

## Content outline

### Introduction (~150 words)

- **Hook:** Most "start an esports team" guides begin with a logo and a sponsor deck. An amateur team begins with four people who can play on Thursday night.
- **Who it's for:** A player putting together a stack for their game's tournament, league or scrims. One sentence sends organisation-builders elsewhere, without a link.
- **Promise:** Deciding what the team is, recruiting for what's missing, tryouts, setting the team up in your format, and keeping it together.
- The TL;DR box goes here.

### H2: Decide what the team is before you recruit

- **Answer first:** Settle three things first, because every post you write and every tryout you run depends on them: the format, the night, and the rank band.
- Cover:
  - Choose the format: Premier, Clash, Battle Cup, ESEA, Siege Cup, the ALGS Circuit, ETF2L or RGL, or scrims only. Link to the join guide's format section for what each one is.
  - Pick one or two fixed evenings. A team that can't agree on a night isn't a team yet.
  - Set a rank band about a tier and a half wide, from the median figure above.
  - Pick one language for voice.
- **Key stat:** median rank range widths from TeamTavern's groups.

### H2: Recruit for the roles you're missing

- **Answer first:** Name the roles you're missing, not every role. Players pick the posts that name their gap.
- Cover:
  - The group post:
    - "3 players, wants 2 more"
    - the missing roles
    - the rank range
    - voice
    - your hours and region
    - "Scrims and tournaments", or your format, under Looking for
  - Two sentences on what the team is like, then a link to the sibling guide "Writing an LFG post that gets answers".
  - Don't only wait for replies. There are ten player posts for every group post, so message the players who fit.
  - Scarce roles take longer. Say what you offer the player.
- **Chart:** grouped bars, groups asking against players offering, for CS2, Dota 2, League, Siege and Overwatch. It's the same data as the join guide's chart, framed for the captain. Reuse the component and change the caption.
- **Key stat:** 30% of group posts tick every role; one in five doesn't mention voice.

### H2: Run a tryout that tells you something

- **Answer first:** Put every candidate through the same test: a voice call, then a scrim or a few ranked games together, then a trial week if it goes well.
- Cover:
  - Test what the team actually needs: the role, calls under pressure, showing up on time.
  - Tell candidates how and when you'll decide.
  - From the research: frequent, selective in-game communication goes with better performance ([Raetze et al., Team Performance Management, 2025](https://www.emerald.com/tpm/article/31/5-6/364/1256464/Taking-aim-at-research-on-esports-teams-a)). Listen for calls that are short and specific.
- Keep it short. PaxJax and Tournova both cover this, so add only the TeamTavern-specific points and the research.

### H2: Set up the team in your game's format

- **Answer first:** Every format has its own way to create a team and its own roster rules. Read them before your first match, not after.
- **Table: roster rules.** One row per format. Columns: who creates it, roster size, subs, what locks and when, cost. Rows from "Captain mechanics" below. Confirmed facts only; cells whose facts aren't confirmed say "check the organiser's rules".
- Under the table, one short paragraph per rule that bites, from the list under Information gain.
- New teams get their own divisions:
  - ETF2L Fresh, free, for players with no competitive experience
  - RGL Newcomer

### H2: Find scrim partners

- **Answer first:** Scrims come from other captains. Set a regular slot and ask the same teams back.
- Cover:
  - TeamTavern group posts that tick "Scrims and tournaments": 62% in League and 54% in CS2 (from the join brief's data).
  - Agree on the rules before the scrim: the map, how many rounds or games, the server or lobby host.
  - Keep a short list of teams near your level.
- The research found no good official sources on scrims. Keep this section to practical steps and claim nothing that would need a citation.

### H2: Keep the roster together

- **Answer first:** Teams fall apart when players stop believing in them, not over one lost match. Low commitment and doubts about the team's future are what predict players wanting to leave ([Raetze et al., 2025](https://www.emerald.com/tpm/article/31/5-6/364/1256464/Taking-aim-at-research-on-esports-teams-a)).
- Cover:
  - Keep a sub or two: Premier takes 5–7 players, and FACEIT's Overwatch teams have 3 sub slots.
  - Split the jobs: roster, scrims, registration.
  - Write down who makes calls in game and out, how prize money is split, and how someone leaves.
  - Make roster changes before the format's lock, not after (link back to the table).
- **Optional:** a Dota study of millions of matches found players more active alongside friends ([Zeng et al., arXiv 2018](https://arxiv.org/abs/1812.02272)). It's a preprint, so cite it as one or leave it out.

### H2: When to go looking for a pro path

- Two or three sentences. The team climbs the format's ladder first; the join guide's section "From amateur team to pro" names each ladder. Link there. Don't repeat it.

### FAQ (5)

1. **How many players do you need?**
   - Five to play in most games; three in Apex; six or nine in TF2.
   - Keep one or two subs.
   - Premier allows up to 7, and RGL Sixes up to 12.
2. **How much does it cost?**
   - Premier and ETF2L are free.
   - Clash needs a ticket from every player.
   - ESEA needs a League Pass per player.
   - RGL's paid divisions charge a fee.
   - Siege Cup needs the paid game since Siege X.
   - Give no prices; none were confirmed.
3. **Do you need a high rank to start one?**
   - No. Premier and Clash place a team by its players' ranks.
   - The higher tiers have gates: OWCS's open qualifier needs Masters 1, and FACEIT leagues don't.
4. **Should an amateur team have a written agreement?**
   - A shared note is enough: who decides, how prizes split, how someone leaves.
   - Not legal advice.
5. **Where do I find players?** Group posts on TeamTavern, plus messaging player posts that fit. Link each busy feed.

### Conclusion (~120 words)

- Takeaways: decide first, recruit for gaps, test the same way, learn the lock rules, write it down.
- **Call to action:** "Post your team" with a link to each busy feed, where the group post form is.

## Captain mechanics, with sources

Put only confirmed facts in the table. Checked 2026-09-28.

- **Premier:**
  - Owner and captain rights, 5–7 players, one team per player, name 5–15 and tag 1–5 characters: [Riot, 2025-07-07](https://support.riotgames.com/en-us/valorant/gameplay/premier-team-creation-management)
  - The MMR rule after the first match, the removal warning, and ownership passing to the longest-standing member: [Riot, 2026-03-30](https://support.riotgames.com/en-us/valorant/gameplay/premier-joining-leaving-teams)
  - Playoff roster lock and 600 Premier Score: [Riot, 2025-12-22](https://support.riotgames.com/valorant/gameplay/premier-playoff-tournaments)
- **Clash:** the captain adds and removes members and sets name, logo and tag; invites open the Monday before; tier is a weighted average leaning on the strongest players; lock-in windows by tier: [Riot FAQ, 2026-05-07](https://support.riotgames.com/en-us/league-of-legends/events/clash-faq)
- **Battle Cup:**
  - Free with Dota Plus, otherwise a ticket: [Valve, 2018-03-13](https://store.steampowered.com/oldnews/38154)
  - Five tickets per team, 8-team bracket: a 2016 Valve post seen only as a snippet. Keep this row thin.
- **ESEA (FACEIT):**
  - Ready once 5 players hold a League Pass
  - 3 Access Tokens for a one-match stand-in
  - after registration closes, at most 3 joins per 7 days
  - the rule for keeping the slot (2 of last season's players in 3 of the first 4 matches)
  - Sources: [FACEIT support](https://support.faceit.com/hc/en-us/articles/9718528520860-How-do-I-register-and-play-in-the-ESEA-League), [hijack policy](https://support.faceit.com/hc/en-us/articles/9731358019868-What-is-the-league-hijack-policy-ie-what-do-I-need-to-do-to-retain-a-team-s-slot-in-a-higher-division-in-the-ESEA-League). These come from search snippets only, since the pages return 403, so **check them in a browser before publishing.**
- **ALGS:** Battlefy registration per event, 3–4 competitors plus a coach, points follow the top 3 players: [ALGS Year 6 rules](https://algs.ea.com/year-6-rules.pdf), seen as snippets only, so **check before publishing.**
- **Siege Cup:** the squad leader registers a five; the team can't be edited after registering; subs only before registration: [Ubisoft FAQ, 2024-05-11](https://www.ubisoft.com/en-gb/game/rainbow-six/siege/news-updates/2qZMThOIMmXBWOmBZS3iSI/siege-cup-faq). This was written for the beta, so check it in the client.
- **Challengermode (R6 Challenger Series):** the captain invites and players accept; mid-event subs need an admin's approval: [Challengermode](https://support.challengermode.com/en/start-here/how-to-create-a-team)
- **Overwatch on FACEIT:**
  - 5 active slots, 3 subs and a coach; FACEIT leagues open from age 13 with no rank limit: [FACEIT guide](https://support.faceit.com/hc/en-us/articles/12473713109020-OW-Team-Creation-Guide) (snippet)
  - OWCS roster of up to 8, age 17+, Masters 1 for the open qualifier: [Blizzard OWCS FAQ](https://esports.overwatch.com/en-us/faq)
- **ETF2L:**
  - team created from team admin; invitees verified after 24 hours
  - 6 to sign up, 5 to play
  - two team changes per player per competition
  - free
  - Fresh division
  - Sources: [ETF2L Newcomer Guide, 2025-12-09](https://rules.etf2l.org/docs/newcomer-guide/), [6v6 rules](https://etf2l.org/6v6-rules/)
- **RGL:**
  - a designated leader, whom a two-thirds vote can replace
  - Sixes roster 5–12
  - rosters lock 2 weeks before the regular season ends
  - fees in paid divisions
  - Newcomer division
  - Sources: [RGL 1006, 2026-09-08](https://docs.rgl.gg/rules/global/1006/), [3002](https://docs.rgl.gg/rules/sixes/3002/), [1007 Fees](https://docs.rgl.gg/rules/global/1007/)

**Not confirmed.** Leave these out:

- Clash subs, positions and bans
- how a Battle Cup team is formed in today's client
- ALGS subs in Year 6
- OWCS "6–7 players"
- R6 Challenger Series "5 + 2 subs" and "3 of 5 core" for 2026
- the ESEA maximum roster size and lock date
- every price

## Statistics to include

| # | Statistic | Source | Date | Section |
|---|---|---|---|---|
| 1 | 26,831 player posts to 2,538 group posts | TeamTavern posts (rerun on production) | 2026-09-26 copy | Recruit |
| 2 | Role demand and supply by game | TeamTavern posts | same | Recruit (chart) |
| 3 | 30% of group posts tick every role; 52% name one or two | TeamTavern posts, excluding Valorant and CS2 | same | Recruit |
| 4 | 79% of group posts state voice; 53% give hours | TeamTavern posts | same | Recruit |
| 5 | Median group rank range: 5 divisions in Valorant, 6 in League | TeamTavern posts | same | Decide |
| 6 | Review of 92 studies: cohesion improves cooperation and satisfaction; low commitment and doubts about the team's future predict wanting to leave | [Raetze, Staedter & Hüllmann, Team Performance Management 31(5/6), 2025-05-30](https://www.emerald.com/tpm/article/31/5-6/364/1256464/Taking-aim-at-research-on-esports-teams-a) | 2025 | Tryouts, Keep together |
| 7 | Communication training changed how university Overwatch teams made calls, by their own report | [Maier, Journal of Electronic Gaming and Esports 2(1), 2024](https://journals.humankinetics.com/view/journals/jege/2/1/article-jege.2024-0036.xml) | 2024 | Tryouts (optional) |

## Competitive gaps to exploit

1. **Roster mechanics.** None of the top five says how any format creates a team, what locks, or what a late change costs.
2. **Beginner divisions and paid entry.** ETF2L Fresh and RGL Newcomer, and which formats cost money.
3. **The recruiting data.** Scarce roles, and what group posts leave out.
4. **The right reader.** Three of the top five are about founding an organisation, and CorrosionHour still names CS:GO and the Overwatch League.

## Internal links

- **Links from this guide:**
  1. The join guide, twice: from the first H2 for the formats, and from the pro-path H2 for the ladders. Anchors: "each game's team format", "the ladder from amateur to pro".
  2. The busy feeds, where the group post form is: `/games/valorant`, `/games/league-of-legends`, `/games/overwatch`, `/games/dota-2`.
  3. The CS2, Siege, Apex and TF2 feeds, from their rows or paragraphs.
  4. "Writing an LFG post that gets answers", once it exists.
  5. The Premier and Clash guides, once they exist.
- **Links to this guide:**
  1. The join guide's "Or start your own team" section.
  2. The `/guides` index.
  3. Later, the Premier and Clash guides.
- **Where it sits:** a spoke of the join guide's hub. The two are a pair, so the join guide links here in its body, not only in a footer.

## Trust signals and schema

- Article with the TeamTavern Organization as author and publisher, BreadcrumbList Home > Guides > How to make an esports team, and `datePublished` and `dateModified`. No Person.
- Every rule links to the organiser's own page. Rules seen only as a snippet are checked in a browser before publishing, or left out.
- The data section is first-hand evidence. Write no "when we ran a team" stories unless the user supplies them.

## Keeping it current

Review, and change the Updated date only when a fact changes:

- **Valorant:** each Premier Stage.
- **League:** Clash, when Riot changes the FAQ.
- **CS2:** each ESEA season.
- **Apex:** each ALGS year.
- **Overwatch:** OWCS registration each January.
- **TF2:** ETF2L and RGL, each season.
- **TeamTavern's numbers:** once a quarter, with the join guide.

## Distribution

The same plan as the join guide:

- ask for indexing in Search Console on publish
- link it from the join guide and the guides index
- after 4–6 weeks, check whether the "make/create an esports team" queries move from the home page to this URL
