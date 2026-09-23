---
name: game-seed
description: Research a game as it is today and write or refresh its TeamTavern seed file (game, game_contact, field, field_option, tracker rows). Use this whenever a game is being added to the catalogue, whenever an existing game's ranks, roles, modes, platforms or trackers need checking or updating (a new season, a rank rework, a console port, a rebrand), and whenever someone asks whether a game's seed is still accurate — even if they only say "add Deadlock" or "Apex changed its ranks again" without mentioning seeds.
---

# Writing a game seed

A game on TeamTavern is one seed file plus one cover. The seed says which
accounts a post in that game can offer, which fields a post answers and with
which options, and which stat sites a game account links to. Matching, the
cards, the post screen and the description bar are all driven by it, so a
wrong option is a wrong card for every player of that game.

The catalogue rots quietly. Of the ten games refreshed in September 2026,
every one had changed since its seed was written: ranks gained tiers and
divisions, modes were removed, two games were renamed, two came to consoles,
and a tracker site shut down. So the job is **research first, then write** —
never a port of what the old file says, and never from memory alone.

## Where things are

- The seed directory is `src/TeamTavern/Database/Seed/`, one file per game
  under `Games/`, with `Regions.sql` and `Countries.sql` beside them.
- The schema is `src/TeamTavern/Database/TablesCurrent.sql`. Its comments on
  `game_contact`, `field`, `field_option` and `tracker` define what `ordered`,
  `slotted`, `applies_to` and `on_card` mean. Read them before writing; the
  rules below assume them.
- `Games/Valorant.sql` in the seed directory is the pattern: follow its SQL
  idiom (`cross join (values ...)`, `join (values ...) on option.field_key =
  field.key`), column order, alignment and header-comment shape exactly.
- Sections 5, 5.4, 5.5 and 7.2 of `redesign/brief.md` say what the fields are
  for. The brief is long; grep for the headings and read those ranges.
- Covers are `src/TeamTavern/Client/Static/Images/Games/<handle>.webp`,
  600x900 WebP with the game's logo legible at grid size.

## Workflow

1. Read the schema comments, the Valorant pattern, the game's existing seed if
   there is one, and the brief sections above.
2. Research the game's current state (next section). Keep a note of the source
   for every fact you'll rely on.
3. Write the seed.
4. Verify it: `bash .claude/skills/game-seed/scripts/verify-seed.sh <file>`,
   then `--all` to prove it composes with the rest of the catalogue.
5. Grep the brief for every option the game had before and every option it has
   now, and report any example in the brief that names an option you removed.
6. Report (last section).

Several games can be done in parallel, one agent each: the files are disjoint
and the verify script gives every run its own database. Agents working apart
drift apart, so a parallel run ends with one pass over the whole catalogue:
`verify-seed.sh --all` prints every game's Looking for options and roles side
by side, and the same idea under two names, or a game with an intent the
others give and it leaves out, is fixed before the seeds are accepted.

## Research

Search for evidence dated within the current season, not the most confident
article. Guides from two years ago rank highly and describe a game that no
longer exists. Confirm:

- **The name.** Games rebrand in both directions: Overwatch 2 went back to
  plain Overwatch in 2026, and Rainbow Six Siege dropped its "Siege X" branding
  after nine months. The request itself may carry a stale name; check it rather
  than trusting it.
- **Platforms, and crossplay between them.** Ports arrive: Valorant and
  Valheim both reached consoles recently. Crossplay rules decide whether a
  platform field actually separates players.
- **Every ranked ladder** — its tiers, its divisions and how players write
  them, which tiers have no divisions, and whether a separate ladder (per-role,
  per-mode, third-party like Faceit) is still live.
- **Roles**, and **what people are after**, from the players' own posts (next
  section), never from the game's official taxonomy alone.
- **Modes**: which are permanent, which rotate, which were removed, and which
  are team formats people recruit for by name.
- **Tracker sites**: alive, and whether their profile URL is a prefix the
  account id completes.

**Check, don't hedge.** If a fact matters and you're unsure of it, look it up
before reporting. "The region may now be called X" is not a finding; "the
matchmaking region is still Dubai, region code 8192" is.

**Say when you couldn't reach a source.** Tracker sites and wikis often return
403 to scripted requests (Cloudflare, Fandom). When a URL shape comes from
search results rather than a page you loaded, say so, so a human can click it
once before launch.

### Evidence from players

The role list and the Looking for options are what players write when they
look for each other, so they come from a sample of those posts, counted. What
a publisher calls its roles and what players call them can differ: Valorant
players write the agent classes, while Apex players name legends rather than
classes, and in Overwatch "DPS" outnumbers "Damage" by twenty to one. Where a
slang name only ties the official one ("smokes" and Controller), the official
one stays. An official list is the answer only when the posts
use it, and labels follow the posts' words.

- **LFG posts.** Sample a few hundred posts from the last two months of the
  game's LFG subreddit or recruitment board, and count how they state a role
  and what they are after. Reddit refuses WebFetch and scripted requests, but
  serves a browser, so `scripts/reddit-posts.cjs` fetches through Playwright's
  Chromium (run it with `"$(volta which node)"`; usage is in its header). The
  active LFG subreddits are `valorant_lfg`, `apexlfg`, `Overwatch_LFG` and
  `TeamRedditTeams` for League; their flairs are regions and platforms, never
  what a post is after, so the counting is on the text. Dota, CS2, Siege, TF2,
  Valheim and Heroes have no live LFG subreddit and recruit on Discord and
  their own boards, so for them the evidence is the recruitment boards
  (ETF2L's, CS2 LFT boards), team announcements and TeamTavern's own posts,
  and the report says the sample is thin.
- **TeamTavern's own posts.** The development database holds the production
  dump, whose about and ambitions texts are years of players describing
  themselves in their own words, not the old form's options.
  `scripts/count-terms.sh <old-handle> <label>=<regex>...` counts the profiles
  mentioning each term, overall and in the last two years.

Report the counts beside the choices they support. A field or an option with
no evidence behind it is a guess, and says so.

## Modelling the fields

### Every game

- **Looking for**: exactly one field, key `looking-for`, label `Looking for`,
  `multi`, `applies_to` all three types, `on_card` true. Its options are what
  someone is *after*, not a list of queues, and they are the same across the
  catalogue. Every game starts from the shared intents, in this order, with
  these keys and labels, and leaves out only the ones its game has no such
  play for (Valheim has no ranked play):

  | Key                  | Label                  | Takes in                                   |
  | -------------------- | ---------------------- | ------------------------------------------ |
  | `casual`             | Casual                 | unranked, quick play, normals, fun modes   |
  | `ranked`             | Ranked                 | every ranked queue, whatever the game calls it |
  | `scrims-tournaments` | Scrims and tournaments | scrims, leagues, cups, organised team play |
  | `learning-the-game`  | Learning the game      | new and returning players, teaching        |

  These four recur in every competitive game in both TeamTavern's posts and
  other LFG tools, under these names. A game's own mode is folded into the
  intent it serves: ARAM, Wingman, Mixtape and Stadium are Casual or Ranked.
  Wanting regular teammates or new friends is not an option either, though a
  tenth of posts say it: they say it beside Casual or Ranked, and it is what
  every post on the site is for.

  After them come the game's **team formats**, each of which has to be all
  three: an official format that is permanent or on a regular schedule, played
  by a team formed ahead of time, and recruited for by name in LFG posts ("LF
  team for Premier"). Valorant's Premier, League's Clash, Dota's Battle Cup
  and TF2's Mann vs. Machine are; a rotating mode or event never is, since a
  post outlives it. A game without queues (Valheim) has playstyles in their
  place, PvE, Building, PvP, Roleplay, held to the same evidence.
- **Platform**: key `platform`, `multi`, `applies_to` all three, `on_card`
  true — only when the game runs on **more than one platform family**. The
  families are PC, PlayStation, Xbox and Switch; every PC store is PC, and
  Switch 2 is Switch. A single-family game has no platform field at all.
- Keep the existing `handle`: the cover is named after it. If the handle has to
  change (CS:GO became `cs2`), the cover must be renamed with it, and say so.

### Ranks

`single`, `ordered` true, `applies_to` `{player,group}` — a community has no
rank. Rank closeness counts steps between options, so the options decide what
"near" means:

- **Include divisions where players speak in them** ("Diamond 2", "Gold II").
  Without them a rank range can't say "Platinum 1 – Diamond 3" and one step is
  a whole tier.
- **Ordinals run worst to best even when divisions count down.** LoL's IV to I,
  Overwatch's 5 to 1, Siege's V to I: Gold IV has a lower ordinal than Gold I.
- **Follow the game's own notation**: Roman numerals where the game writes
  them (LoL, Apex, Siege), arabic where it does (Valorant, Overwatch, Dota's
  stars, HotS). Keys mirror labels: `gold-ii`, `diamond-2`. This holds even
  where posts write it otherwise, as League players write "Emerald 3": a rank
  is read off the game's screen, unlike a role, whose words are the players'.
- **Top tiers usually have no divisions** — Radiant, Master and above in LoL,
  Champion in Overwatch, Immortal in Dota, Rookie and Master in Apex. Check each.
- **A numeric rating becomes uniform bands**, labelled the way players say it.
  CS2's Premier rating is a number in the thousands that the game paints in
  seven colours 5,000 wide; one step of 5,000 spans most of the ladder, so the
  field is 1,000-wide bands labelled `12k`, which is exactly what "I'm 12k"
  means. A coarse official band is a badge, not a scale.
- **One primary rank is `on_card`; secondary ladders sit behind Details**
  (Wingman, Faceit level, Battle Cup tier, Clash tier). The exception is
  per-role ranks that are peers, as in Overwatch: all three go on the card,
  because a post fills only the ranks of the roles it plays.
- **A ladder that still ships stays, even if nobody queues it** — TF2's Valve
  rank has an empty queue and is still a field, behind Details. What goes is a
  ladder whose mode was *removed*: Apex's Arenas rank, CS2's Danger Zone rank,
  TF2's Faceit fields once Faceit dropped the game.

### Roles

`multi`, `slotted` true, `applies_to` `{player,group}`, `on_card` true.
Slotted means two players fit by covering *different* options, so the list has
to be one where that's true: slots a team fills.

- A game has a role field only when its LFG posts state roles. Where they name
  characters instead and a class list is nobody's vocabulary, there is no role
  field, or the one organised play uses (Apex's competitive posts say fragger
  and support).
- The options are the words the posts use (Evidence from players): agent
  classes in Valorant, positions in Dota, jobs in CS2 and Siege. Labels follow
  them too, "DPS" where players write DPS.
- Name the field what the game does: `role`, `class` (TF2), `position` (Dota,
  where players say "pos 4"). The client keys off `slotted`, not the key.
- **Only slots.** A job a player takes on top of their slot is not an option:
  an in-game leader still plays entry or anchor, and two players who both lead
  are not short of anything the slot rule can see. Sub-roles that are hero
  passives rather than something you queue as are not options either.
- **A job on top of the slot** gets a field of its own where the community
  names it and teams recruit for it by name (team posts weigh most): the
  in-game leader in CS2, Valorant and Siege, and whatever a new game's players
  call such a job, a captain or a shotcaller, under their word. It is
  `boolean`, `slotted` false, `applies_to` `{player,group}`, `on_card` false,
  with no options, keyed and labelled as the community says it
  (`in-game-leader`, `In-game leader`). It says a player can take the job, not
  that it is all they play, so two players fit when either takes it, as the
  schema's comment on `ilk` says. A game whose community doesn't name such a job, as Dota's
  doesn't, has no such field.
- **No Flex, Fill or Any.** A player who plays anything picks every option,
  which is what covering any slot means under the slot rule; an option of its
  own would match nothing.

### Other game fields

- `applies_to` all three types where a community answers it about itself
  (Valheim's server type, TF2's server type and format), `{player,group}`
  otherwise. `on_card` false unless the fact leads the card — Valheim's server
  type does, which is how a Valheim community says what it is.
- **Server region is not the post's region.** Every post already carries the
  brief's twelve regions for *where players are*. A game field about which game
  server a post plays on is different and can stay, keyed `server` so it can't
  be read as location (Dota). Fold a publisher's internal sub-servers into the
  region a stranger would name.

## Contacts and trackers

- `game_contact` holds the accounts a post in this game can offer, from the
  schema's list. `discord` is on every game. Offer the identity players of this
  game actually exchange: Overwatch players trade BattleTags even on Steam; a
  Siege account is a Ubisoft account on every store. A console port brings its
  platform's account: `psn`, `gamer_tag`, `friend_code`.
- A tracker's `template` is a URL **prefix** the account id is appended to, and
  its `contact_kind` must be one of the game's contacts. Region-scoped sites
  don't fit (u.gg, porofessor, heroesprofile, hotscompanion), and neither do
  sites keyed by an internal numeric id. A site's *search* route sometimes fits
  where its profile route doesn't: op.gg's search takes a bare Riot ID and
  redirects to the right region. Drop dead sites (overbuff.com closed). A game
  with no site that fits gets no tracker rows, which is fine.

## The file

- Header comment: present tense, only the decisions a reader couldn't infer —
  why ranks carry divisions, why a ladder was kept behind Details, why a field
  is slotted, why there is no platform field. No dates, no "used to be", no
  changelog; the git log holds those. Follow the Valorant file's shape.
- LF, no trailing whitespace, aligned `values` columns.
- A description in the site's voice: `Find <Game> players, groups and
  communities: ...` naming what people there look for.

## A new game, not a refresh

A new game also needs its cover before it can ship — the home page grid, the
header dropdown and the post screen all show it, and `games.spec.ts` fails on
a tile whose cover doesn't load at 600x900. Steam's `library_600x900_2x.jpg`
is that shape for games on Steam; SteamGridDB carries it for the rest. The
logo on it has to name the game legibly at tile size, since the grid shows no
titles. The verify script lists any game without a cover.

## Report

Report so a human can review decisions without redoing the research:

1. **The fields**, as the verify script prints them, and the contacts and
   trackers.
2. **Every change against the previous seed**, each with its source. For a new
   game, every non-obvious choice with its source. Roles, Looking for and team
   formats carry the post counts that support them.
3. **What is a construction rather than a fact.** Anything you assembled that
   the game or its scene doesn't publish as such must be labelled as yours —
   TF2's division field merges ETF2L's and RGL's ladders onto one scale, and
   neither league publishes that mapping. The first question a reviewer asks
   is "does this exist in-game, or did you make it up?"; answer it before
   they ask.
4. **Conflicts with the brief**: any example in `redesign/brief.md` that names
   an option the seed no longer has.
5. **What you couldn't verify**, and what needs a human call.
