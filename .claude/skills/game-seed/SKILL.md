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

- The seed directory holds one file per game under `Games/`:
  `redesign/seed/Games/` while the redesigned schema still lives in
  `redesign/schema.sql`, `src/TeamTavern/Database/Seed/Games/` once it has
  replaced `Database/TablesCurrent.sql`. The verify script picks whichever is
  current.
- The schema's comments on `game_contact`, `field`, `field_option` and
  `tracker` define what `ordered`, `slotted`, `applies_to` and `on_card` mean.
  Read them before writing; the rules below assume them.
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
and the verify script gives every run its own database.

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
- **Roles** as players of this game describe themselves in an LFG post.
- **Modes**: which are permanent, which rotate, which were removed.
- **Tracker sites**: alive, and whether their profile URL is a prefix the
  account id completes.

**Check, don't hedge.** If a fact matters and you're unsure of it, look it up
before reporting. "The region may now be called X" is not a finding; "the
matchmaking region is still Dubai, region code 8192" is.

**Say when you couldn't reach a source.** Tracker sites and wikis often return
403 to scripted requests (Cloudflare, Fandom). When a URL shape comes from
search results rather than a page you loaded, say so, so a human can click it
once before launch.

## Modelling the fields

### Every game

- **Looking for**: exactly one field, key `looking-for`, label `Looking for`,
  `multi`, `applies_to` all three types, `on_card` true. About four options.
  These are what someone is *after* — Casual, Ranked climb, Tournaments,
  Events, Roleplay — not a list of queues. Fold queue clutter into the intent
  it serves (unranked and quick play are Casual). Leave out modes that rotate
  in and out, since a post outlives them. A mode with its own crowd can earn a
  slot (ARAM, Mixtape, Mann vs. Machine), and so can an intent peculiar to the
  game ("Learning the game" for Siege's steep curve).
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
  stars, HotS). Keys mirror labels: `gold-ii`, `diamond-2`.
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

- Name the field what the game does: `role`, `class` (Apex, TF2), `position`
  (Dota, where players say "pos 4"). The client keys off `slotted`, not the key.
- Use the game's own current list where it has one (Valorant's four agent
  roles, Apex's legend classes, HotS's six roles, Overwatch's Tank/Damage/
  Support), and playstyle roles where that's how players self-describe (CS2's
  entry/AWPer/lurker, Siege's entry/anchor/roamer).
- Sub-roles that are hero passives rather than something you queue as are not
  options.

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
   game, every non-obvious choice with its source.
3. **What is a construction rather than a fact.** Anything you assembled that
   the game or its scene doesn't publish as such must be labelled as yours —
   TF2's division field merges ETF2L's and RGL's ladders onto one scale, and
   neither league publishes that mapping. The first question a reviewer asks
   is "does this exist in-game, or did you make it up?"; answer it before
   they ask.
4. **Conflicts with the brief**: any example in `redesign/brief.md` that names
   an option the seed no longer has.
5. **What you couldn't verify**, and what needs a human call.
