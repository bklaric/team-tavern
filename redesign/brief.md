# TeamTavern redesign: UX brief

TeamTavern is relaunched with a new UX and a new design system. This is not a
reskin: flows, page content and the data model are all free to change. This brief
covers the listings, which everything else follows from: what players browse,
how they post, how they are found and how they get in touch. Prototypes are built
from it, and the design system is extracted from the prototypes.

Every item carries a status:

- **Decided**: settled; prototypes follow it.
- **Proposed**: the current best answer; prototypes use it, and it can still change.
- **Open**: not settled; prototypes may try alternatives.

## Glossary

- **Post**: what a player publishes for one game. Player, group or community.
- **Group**: a few people who play together and need more players. The word
  "team" is gone from the site; a group is not an entity and has no members.
- **Community**: a clan, server or Discord open to many members.
- **Description**: what a viewer says about themselves on a feed, using the same
  fields a post has. It doubles as a post draft.
- **Feed**: a game's single listing page, holding all three post types.
- **Active and expired**: a post is active for 30 days after it is published,
  renewed or edited (90 for communities), and expired after that. Both stay in the
  feed, split by a divider.
- **Match**: how many of a post's fields agree with a description or another post.
  Matching is fuzzy and never excludes.
- **Tier**: the posts in a feed that match the same number of fields, shown under
  one heading.
- **Renewal**: one click in an email, the **Renew** button on the home page, or an
  edit, which makes a post active again.

## 1. Why

There are no analytics: none were collected, and the site has been unindexed for
over a year. The evidence is the production database, restored locally from the
12 September 2026 dump, plus experience running the site.

### What the data shows

**Nobody comes back.**

- 86% of players (26,909 of 31,196) have exactly one session.
- 7% signed in again 30 or more days after registering.

**Profiles are written once at registration and never touched.**

- 98% of player profiles are created within a day of registering, 92% within 10 minutes.
- 11% are ever edited.
- 97% of players with a profile have exactly one.

**Listings are almost entirely stale.**

| Game     | Profiles | Updated in last 30 days | Updated in last year |
| -------- | -------- | ----------------------- | -------------------- |
| Valorant | 13,420   | 27                      | 319                  |
| LoL      | 3,639    | 11                      | 182                  |
| Dota 2   | 3,204    | 16                      | 229                  |
| Valheim, Splitgate, HotS | 415 | 0                   | 8                    |

A visitor who contacts someone from a listing is almost always contacting someone
who left long ago.

**Structured data is reliable; free text is thin.**

- Birthday, languages, location and timezone are 92–97% filled, even though the
  details step can be skipped.
- Online times are 56% filled.
- Game fields are 83% filled on average.
- 21% of profiles have an empty about, 35% empty ambitions. The median about is
  105 characters: short and practical ("peak diamond, returning after a break",
  "LF non-toxic mates for comp"), in many languages.

**Teams are mostly small groups, not organizations.**

- 26,881 player profiles against 3,076 team profiles.
- Team profiles: 1,954 informal parties, 641 organized parties, 299 organized
  communities, 182 informal communities.
- 94% of team owners own one team; 2,924 teams have exactly one game profile;
  1,200 teams (29%) have no profile at all.
- 695 team owners also have a player profile for the same game.

**Alerts are the only thing signed-out visitors use.**

- 3,980 alerts from 3,540 distinct emails; only 603 of those emails belong to
  registered players.
- They average 4.9 criteria. Location (92%), fields (88%), languages (84%) and age
  (81%) are used most; online times least (41%).
- An alert emails once per matching newly created profile; edits never trigger it.

**Traffic.** Registrations ran at about 550 a month until May 2025 and have been
about 85 a month since June 2025. A quarter of registrations use Discord.

### The problems this brief addresses

1. Nothing brings a player back. Contact happens off-site, so a profile owner never
   learns whether their profile worked.
2. Listings misrepresent activity: thousands of dead profiles bury the few live ones.
3. Cards show everything a profile holds, so browsing means reading full profiles.
4. Players and teams are split into two listings, so a solo player checks both, and
   "team" hides two different things: small groups and communities.

## 2. Principles

- **Listings first.** The feed, the cards and posting come first; the rest of the
  site follows from them.
- **Searching is posting.** To find players, groups and communities, a player
  describes themselves, and that description is their post, a few clicks from
  published.
- **Structured facts lead, free text supports.** Cards and matching are built on the
  data players reliably give.
- **Freshness is visible.** A player can always tell a live post from an old one.
- **Contact happens where it can be seen.** Messaging brings both sides back and
  gives owners feedback.
- **Relaunch, not migration.** The site is relaunched as a whole. Losing some data
  in the migration is acceptable, given current activity.
- **Don't solve problems we don't have.** No rate limits, anti-bumping rules or
  anti-abuse machinery until the problem shows up.

## 3. Concepts

### Posts

A **post** is what a player publishes for one game. There are three types:

| Type          | Is                                            | Looking for          | Answered by      |
| ------------- | --------------------------------------------- | -------------------- | ---------------- |
| **Player**    | One person                                    | A group, a community, or someone to duo with | Message or contacts |
| **Group**     | A few people who need more players            | Players              | Message or contacts |
| **Community** | A clan, server or Discord open to many        | Players              | Join or message  |

- **Decided:** groups and communities are posts owned by a player, not entities.
  They have no members, no shared management and no page beyond the post.
- **Decided:** a player has at most one post of each type per game. A player can
  have a Valorant player post, a Valorant group post and a Dota player post, but
  not two Valorant player posts.
- **Decided:** a player with an existing post of a type edits or deletes it; a new
  post never silently replaces it.
- **Decided:** every post is listed, and every post notifies its owner when a post
  that fits it appears. There are no separate alerts: being findable is the price
  of being notified (section 8).

### Group or community: joining people or joining a place

The line between a group and a community is not size or how the game is played.
It is whether you join **people** or a **place**:

|                 | Group (joining people)                        | Community (joining a place)              |
| --------------- | --------------------------------------------- | ---------------------------------------- |
| You evaluate    | Specific individuals                          | Culture, activity, rules, events         |
| Capacity        | Finite: "need 2 more"                         | Open-ended, never full                   |
| Lifecycle       | Fills up and closes, goes stale fast          | Ongoing                                  |
| Relationship    | Everyone knows everyone                       | You are one of many                      |
| First step      | A conversation with the owner                 | Join the server, look around             |

This holds for server-based games too. On Valheim, "my wife and I are starting
over on a new server and want a few more players" is a group; "The Farlands
community: open recruitment, 100-player events" is a community. For server games,
a group's open slots mean how many more people the group wants on its server.

**Decided:** the owner picks the type when creating the post, guided by examples of
each. The distinction lives in the post types, their cards and their lifecycles,
not in separate pages.

## 4. The feed

**Decided:** each game has one listing page, the feed, holding player, group and
community posts.

```
┌─────────────────────────────────────────────────────────────────────────┐
│ [cover] Valorant                                                        │
│         Find players, groups and communities                            │
│         38 active posts                                                 │
├─────────────────────────────────────────────────────────────────────────┤
│ (•) I'm a player looking for a group                                    │
│ ( ) We're a group looking for players                                   │
│ ( ) We're a community looking for members                               │
│                                                                         │
│ Diamond 2 ▾  Controller, Sentinel ▾  Croatia ▾  EN, DE ▾  19–23 ▾  🎤   │
│ More ▾                                                                  │
├─────────────────────────────────────────────────────────────────────────┤
│ ✦ Publish this as your post: groups and players can find you too, and   │
│   we'll tell you when someone new fits.               [ Publish post ]  │
├─────────────────────────────────────────────────────────────────────────┤
│ Showing  [ All ]  [ Groups ]  [ Communities ]  [ Players ]              │
│                                                                         │
│ Fits you (3)                                                            │
│  group card      ✓ Needs Controller · ✓ Diamond fits · ✓ EU · ✓ 21–23   │
│  community card  ✓ EU · ✓ EN · ✓ PC                                     │
│  player card     ✓ Diamond 1 · ✓ EU · ✓ 19–22 · ✓ EN                    │
│ Missing one thing (12)                                                  │
│  group card      ✓ Needs Sentinel · ✓ EU · ✓ 21–23 · ≠ Platinum 1–2     │
│ Missing more                                                            │
│  player card     ✓ EU · ≠ Bronze 2 · ≠ Online 08–12 · ≠ PT              │
├──────────── Older posts · they may no longer be looking ────────────────┤
│  player card (dimmed)                        Expired 2 months ago       │
│  group card (dimmed)                         Expired 1 year ago         │
│                           [ Load more ]                                 │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Decided:** the feed has no filters to set. The viewer says whether they are a
  player, a group or a community, and describes themselves with the same fields
  their post would have. The feed shows the posts that fit (section 7). The
  description is a post draft, so publishing it is a few clicks away.
- **Decided:** a player viewer sees groups, communities and players, and narrows
  what is shown with **Showing: All, Groups, Communities, Players**. That changes
  the view, not the description. Group and community viewers see only players, so
  they get no segments.
- **Decided:** an empty description shows every post, ordered by activity (7.1).
- **Decided:** the viewer's own posts are in the feed while the description is
  empty and drop out as soon as it carries anything. A description filled in from
  your own post matches that post best, so it would otherwise stand at the top of
  its owner's feed.
- **Decided:** a post the viewer already has a conversation about stays in the
  feed, marked as such (5.6).
- **Decided:** active posts come first, then a divider, then expired posts in the
  same list. Nothing is hidden. When a game has no active posts, the divider is at
  the top.
- **Decided:** within each of those, posts are split into tiers by how many fields
  match the viewer's description and ordered by last renewal or edit inside each
  tier (7.2). An empty description matches nothing in particular, so the feed is
  simply ordered by last renewal or edit.
- **Decided:** a **Load more** button replaces pagination. Nothing loads
  automatically. Each batch continues after the last shown post's sort position
  (match count and renewal time), not by offset, so posts don't shift between
  batches. A post renewed during browsing moves above the loaded batches and isn't
  shown until the feed is reloaded, and one that expires can show twice; both are
  acceptable.
- **Decided:** renewal is allowed at any time and moves the post up. Bumping has
  never been a problem, so nothing prevents it.
- **Proposed:** the header shows how many posts are active, not the total, which
  overstates activity.
- **Proposed:** pages whose posts are all expired are kept out of search engines.
- **Decided:** cards expand in place (5.4), so browsing the feed doesn't navigate
  away and Back doesn't discard the loaded batches. Opening a post's own page from
  its heading still does, and Back from it returns to the feed intact (11.1).
- **Open:** community cards are larger than player and group cards, so the feed
  mixes card sizes. A prototyping question.
- **Open:** competitions and leagues: things you join where an organizer runs the
  show. They are time-bound (sign-up deadlines, seasons) and usually entered with
  a group, so a league could link to groups recruiting for it. Where they appear
  and who lists them (organizers or curation) is undecided. Not part of the first
  release.

## 5. Cards

**Proposed** for all three cards: a card is a summary for comparing, not a full
profile. Browsing is scanning for the few facts that rule someone in or out.

- The first lines carry the decisive facts, the ones players reliably fill in.
- The player's own words are shortened to a few lines.
- Everything else waits until the card is expanded (5.4).
- Contacts and join links wait behind the card's contact button (5.6), never
  shown up front.

**Proposed:** which game fields count as decisive is set per game, for example
rank and role for Valorant, rank and position for LoL. Either the first fields by
order or a "shown on card" flag per field.

**Decided:** platform, a community's kind (such as a dedicated server) and the
Looking for options (5.5) are game fields: a game has them only where they make
sense, with options of its own. Each field takes one option or several, as suits
the field. A game field such as server region is the same field on all three
post types.

**Decided:** a player gives their location; a group or community gives the
regions of the players it is looking for. Every location maps to a region, which
is how the two are compared (7.2).

**Decided:** cards show online hours in the viewer's timezone, converted from the
owner's, so they read the same way as the ✓ and ≠ marks (5.6).

### 5.1 Player card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ ShadowFox                                  PLAYER    Active 2 days ago  │
│ Diamond 2 · Duelist, Initiator · Croatia · EN, DE · 19–23 CET · 🎤      │
│ [ Ranked climb ]  [ Returning player ]                                  │
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together…"                            │
│                                  [ Add on Discord ]        Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

### 5.2 Group card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Night Owls                                  GROUP    Active 5 hours ago │
│ ●●●○○  3 of 5 · Needs Controller, Sentinel                              │
│ Platinum 1 – Diamond 3 · EU · EN · 21–01 CET · 🎤 · Ages 18+            │
│ [ Ranked climb ]                                                        │
│ "Three friends who play most nights, we want to stop solo queuing for   │
│  the last two spots. No tilt, comms on…"                                │
│ Posted by Kestrel                       [ Message ]        Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** the open slots and needed roles are the headline, where a player
  card has rank. They are what a solo player scans for.
- **Proposed:** ranges and sets replace single values: rank range, age range, one
  or more regions.
- **Proposed:** a name is optional; without one the heading reads "Kestrel's group".
- **Proposed:** "Posted by" names the owner, since conversations are between players.
  It is plain text: there is no player page to link to (11.1).
- **Proposed:** for server games the headline reads "Wants 2–3 more on our server",
  and rank disappears where the game has none.
- **Proposed:** "Organized" is a tag, with the website behind the contact button
  (5.6).

### 5.3 Community card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ The Farlands                                COMMUNITY                   │
│ Dedicated server · EU · EN · PC                   Active 1 week ago     │
│ [ Events ]  [ Casual ]  [ All experience levels ]                       │
│                                                                         │
│ "Looking for a fun and friendly Valheim community server? Join The      │
│  Farlands: weekly boss raids, a trading hub, building contests and      │
│  100-player events. New Vikings get a starter kit and a guide…"         │
│ Run by Eirik                       [ Join Discord ]        Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** the contact button reads **Join Discord**, **Visit site** or
  **Message**, depending on how the community says people join (section 6, step 3).
- **Proposed:** the pitch gets more room than on other cards, because for a
  community the text is the product.
- **Decided:** the Discord invite and website sit behind the contact button like
  other contacts, so opening them requires sign-in.
- **Proposed:** a community carries a Looking for (5.5) and an experience level,
  shown as tags.
- **Out of scope:** community logos. They need uploads and moderation.

### 5.4 Expanding a card

```
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together. I play most evenings after  │
│  work and I'm free all weekend."                                        │
│                                                                         │
│ Age 24 · Agents: Jett, Raze, Sova · Playstyle: Aggressive               │
│ Tracker: tracker.gg/shadowfox ↗                                         │
│                                  [ Add on Discord ]        Details ⌃    │
```

- **Proposed:** a card has one expansion, not two. Expanding shows the full text
  and the remaining fields together; there is no separate **Read more**.
- **Decided:** only the **Details ⌄** button expands the card, not the card itself.
  The card grows in place; the feed doesn't navigate.
- **Open:** on a phone a card either expands in place as it does here or opens in
  a modal. Prototyping decides.
- **Decided:** contacts and join links stay behind the contact button, not in the
  expansion, since they are gated and counted (5.6).
- **Proposed:** the nickname, group name or community name links to the post's own
  page, which is what is shared, crawled and opened in a new tab.
- **Proposed:** what sits behind the expansion, by type:

| Type      | On the card                                                        | Behind Details                                                            |
| --------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| Player    | Rank, roles, location, language, online hours, microphone, Looking for | Age, remaining game fields, trackers                                |
| Group     | Open slots, needed roles, rank range, regions, language, online hours, microphone, ages | Remaining game fields                             |
| Community | Kind, regions, language, platform, Looking for, experience level   | Remaining game fields, the rest of the pitch                              |

### 5.5 Free text and "Looking for"

- **Proposed:** about and ambitions merge into one field, "About you and what you're
  looking for". Ambitions are left empty most often, overlap with about when both
  are written, and are nearly always one of a few answers.
- **Proposed:** a structured **Looking for** on all three post types, one or more
  of the game's options, such as Casual, Ranked climb or Competitive for Valorant,
  and Events or Roleplay for Valheim. Each choice is a card tag, and together they
  are a matching field.
- **Decided:** the options are set per game and are the same for all three post
  types, so a player's answer compares directly with a group's or a community's.
- **Proposed:** "New or returning player" stays, as a tag.

### 5.6 Card actions

- **Proposed:** in the feed, each card lists what fits the viewer's description
  ("✓ Needs Controller", "✓ Diamond fits") and what doesn't ("≠ Platinum 1–2"),
  which is what makes the fuzzy ordering (7.2) legible.
- **Decided:** a post offers both on-site messaging and off-site contacts
  (Discord tag, Riot ID and the other game accounts).
- **Decided:** messaging and revealing contacts both require sign-in.
- **Decided:** each card has one contact button, and every label opens the same
  contact panel. The label follows the preference the owner states on the post
  screen (section 6, step 3):

| Owner's preference                | Button          |
| --------------------------------- | --------------- |
| Message me on TeamTavern          | Message         |
| Add me on Discord or in game      | Add on Discord, or Add in game when there is no Discord tag |
| Either is fine                    | Contact         |
| Community: join with Discord      | Join Discord    |
| Community: join through a website | Visit site      |
| Community: they message me first  | Message         |

- **Decided:** the panel holds the contacts and join links the owner shared, and
  the conversation about the post with a message box. The preference decides which
  comes first. The message box is always there: contact on the site is what brings
  both sides back, and it keeps a post whose owner gave no contacts reachable.
- **Proposed:** opening a panel that shows contacts or join links counts as a
  reveal, so owners can be told how often it happens.
- **Decided:** a card the viewer already has a conversation about says so, and
  its button reads **Open conversation** and opens the panel on it.
- **Decided:** on the viewer's own post, in the feed or on its page, **Edit** and
  **Renew** take the place of the contact button, as on the home page (11.2).
- **Open:** on a desktop the panel is a modal or a side panel; on a phone it is
  full-screen. Prototyping decides.

```
┌───────────────────────────────────────────────────────────┐
│ ShadowFox · Valorant player                             ✕ │
│                                                           │
│ Prefers Discord                                           │
│   Discord   shadowfox                           [ Copy ]  │
│   Riot ID   ShadowFox#EUW                       [ Copy ]  │
│ ───────────────────────────────────────────────────────── │
│ Or message on TeamTavern                                  │
│ [ Write a message…                             ] [ Send ] │
└───────────────────────────────────────────────────────────┘
```

## 6. Post creation

**Decided:** onboarding and post creation are one flow. Signed out, it ends with
registration; signed in, it skips it.

```
Type ─▶ Game ─▶ Post ─▶ Register or sign in ─▶ Matches
(skipped when known)    (signed out only)
```

### Entry points

| From                                        | Starts at                                   |
| ------------------------------------------- | ------------------------------------------- |
| **Publish post** on the feed                | Post, prefilled from the description        |
| **New post** in the header, on a feed       | Type, with the game known                   |
| **New post** elsewhere, or the home page    | Type                                        |
| **Edit** on the home page                   | Post, prefilled from the post               |

- **Decided:** once the type and game are known, a signed-in player who already has
  a post of that type for the game is shown it and offered **Edit it** or **Delete
  it**. Editing opens the post screen prefilled.
- **Decided:** a signed-out player is checked when they sign in, at the register
  step or through **Sign up with Discord** on the post screen. If their account
  already has a post of that type for the game, they choose between updating that
  post with what they just entered and discarding it.

### Steps

1. **Type.** The same choice as on the feed, in the same words, each with a short
   example.

```
┌───────────────────────────────────────────────────────────┐
│  What are you posting?                                    │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ 🧍  I'm a player looking for a group                 │  │
│  │     Groups, communities and other players find you  │  │
│  └─────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ 👥  We're a group looking for players               │  │
│  │     "Three of us play most nights, need a fifth"    │  │
│  └─────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ 🏰  We're a community looking for members           │  │
│  │     "Our server runs weekly events, all welcome"    │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
```

   - **Decided:** the player option keeps "looking for a group", the phrase players
     already use; its example says who finds the post.
   - **Proposed:** no "Just browsing" choice. Browsing starts from a game cover,
     on the home page (11.2) or in the header.
2. **Game.** The cover grid, as in the header and home page.
3. **Post.** One screen holds everything the post needs, beside a live preview of
   the card that fills in as the player types. The preview is the card exactly as
   it will appear, so there is no separate preview step.

```
┌──────────────────────────────────────┬──────────────────────────────────┐
│ Valorant · group                     │ Preview                          │
│                                      │ ┌──────────────────────────────┐ │
│ Group name       [Night Owls]        │ │ Night Owls            GROUP  │ │
│ How many are you, and how many do    │ │ ●●●○○ 3 of 5 · Needs         │ │
│ you want in total?                   │ │ Controller, Sentinel         │ │
│   [ 3 ] of [ 5 ]                     │ │ Plat 1 – Dia 3 · EU · EN     │ │
│ Roles you need   [Controller][Sent…] │ │ 21–01 CET · 🎤               │ │
│ Rank range       [Plat 1] – [Dia 3]  │ │ [ Ranked climb ]             │ │
│ Regions          [EU ▾]              │ │ "Three friends who play most │ │
│ Language         [EN ▾]              │ │  nights, we want to stop…"   │ │
│ Usually online   [21:00] – [01:00]   │ │ Posted by Kestrel [ Message ]│ │
│ Looking for      [✓] Ranked climb    │ │                     Details ⌄│ │
│ Microphone       [✓] Required        │ └──────────────────────────────┘ │
│                                      │                                  │
│ Tell people about your group         │                                  │
│ [Three friends who play most nights… │                                  │
│ Ideas: How do you play? What are you │                                  │
│ aiming for this season?              │                                  │
│                                      │                                  │
│ How should people reach you?         │                                  │
│ (•) Message me on TeamTavern         │                                  │
│ ( ) Add me on Discord or in game     │                                  │
│ ( ) Either is fine                   │                                  │
│ Discord [kestrel]  Riot ID [       ] │                                  │
│ Discord server   [discord.gg/…]      │                                  │
│ Website          [https://…]         │                                  │
│                                      │                                  │
│ ⌄ Add more details (optional)        │                                  │
│   Agents, playstyle                  │                                  │
│                                      │                                  │
│ Your post stays active for 30 days.  │                                  │
│ We'll email you before it expires,   │                                  │
│ and tell you when someone new fits.  │                                  │
│                     [ Publish post ] │                                  │
└──────────────────────────────────────┴──────────────────────────────────┘
```

   The screen has four parts, in the order the card shows them:

   - **The card's fields.** Only the fields the card shows.
     - *Player:* rank, roles, location, language, online hours, microphone,
       Looking for, new or returning.
     - *Group:* optional name, size and open slots, needed roles, rank range,
       regions, language, online hours, microphone, age range, Looking for,
       organized.
     - *Community:* name, kind, Looking for, experience level, regions, language,
       platform.
   - **In your words.** One text field with prompts and an example. **Proposed:**
     optional for player and group posts, required for communities.
   - **Contact.** How people reach the post, prefilled from the account.
   - **Add more details (optional).** Collapsed, holding every field the card
     doesn't show.

   - **Decided:** online hours are one range, "Usually online 21:00–01:00", in the
     player's timezone, and a range may cross midnight. There are no separate
     weekday and weekend hours: online times are the field players leave empty most
     often (56% filled) and the criterion alerts use least (41%), and a second range
     doubles that friction.
   - **Decided:** facts about the player (age, languages, location, timezone) and
     their contacts are asked once and live on the account. This screen asks for
     them where the account doesn't have them yet: location, languages and timezone
     with the card's fields, since the card shows them, and birthday under **Add
     more details**. There is no separate details step. Player posts show the facts
     as the account has them now, and every player and group post the player owns
     shows their contacts; changing either on the account page (11.5) doesn't renew
     any post. Group and community posts carry their own regions, languages and age
     range: regions prefilled from the region of the owner's location, languages
     from the account.
   - **Decided:** a player who already has other posts is told, beside an account
     fact or contact they change here, that the change applies to all their posts.
     A player writing their first post isn't.
   - **Decided:** the preview expands with **Details ⌄**, as cards do in the feed
     (5.4). The card's fields change the card itself; **Add more details** fills in
     only what the expansion shows, which is why it is a separate section.
   - **Proposed:** player and group posts ask "How should people reach you?":
     message me on TeamTavern, add me on Discord or in game, or either, followed by
     the game's contact fields. A group may also give a Discord server invite and a
     website.
   - **Proposed:** community posts ask "How do people join?": a Discord invite, a
     website, or "they message me first". No link is mandatory; 71% of today's
     communities have a Discord server and 30% a website, but some vet applicants
     or run on a game server.
   - **Decided:** the answer sets the card's contact button (5.6). On-site messaging
     exists on every post, so no post is unreachable and every off-site contact is
     optional.
   - **Proposed:** a community's renewal email asks whether the invite link still
     works, since nothing checks it.
   - **Decided:** when signed out, a **Sign up with Discord** button sits beside the
     Discord input. It saves the draft, registers or signs in with Discord, and
     returns to this screen with the Discord tag filled in. A new account's
     register step then asks only for a nickname; signing in to an existing
     account runs the existing-post check (Entry points) and skips the register
     step.
   - **Proposed:** until the player has registered, the preview reads "Posted by
     you".
   - **Proposed:** the renewal and notification rules are stated above **Publish
     post**: "Your post stays active for 30 days. We'll email you before it
     expires, and tell you when someone new fits." Communities read 90 days.
   - **Open:** on a phone the side-by-side preview becomes the card above the
     fields, or a preview opened on demand. Prototyping decides.
4. **Register or sign in** (signed out only). The draft is kept through it.
   - **Decided:** this is the site's one sign-up screen: email, nickname and
     password, or **Continue with Discord**, with a link to sign in instead. The
     header's **Sign up**, and opening a contact panel signed out, lead to the same
     screen, and it returns the player to where they were.
   - **Decided:** registration always asks for a nickname. A player who signed up
     with Discord on the post screen sees only the nickname prompt here; one who
     continues with Discord here goes on to the same prompt. Discord prefills it,
     and the player can edit it.
5. **Matches.** "Your post is live", followed by posts that fit it.

```
┌───────────────────────────────────────────────────────────┐
│ ✓ Your post is live                                       │
│                                                           │
│ 7 players fit your group right now                        │
│ ┌───────────────────────────────────────────────────────┐ │
│ │ player card                                           │ │
│ │ player card                                           │ │
│ └───────────────────────────────────────────────────────┘ │
│ [ See all ]              Add more details to your post →  │
└───────────────────────────────────────────────────────────┘
```

   - **Proposed:** with nothing matching well, the closest posts are shown with
     what doesn't fit, and "We'll email you when someone fits."
   - **Proposed:** a player who left **Add more details** empty is offered it once
     more here.

## 7. Matching

### 7.1 Describing yourself instead of filtering

Plain filters can't tell who is searching. A Controller filtering by Controller
wants groups that need one, not other Controllers: the same value means different
things on a player card and a group card. A description of the viewer carries that
context, so each field can be compared the way the pair of post types calls for.

- **Decided:** the viewer describes who they are, not what they want: a player, a
  group or a community, with that type's post fields. What fits follows from the
  type (7.2).
- **Decided:** the description is one choice of type, never several. One
  description is one post.
- **Decided:** there is no separate browsing mode. An empty description shows every
  post, ordered by activity; each field filled in pulls the posts that match it
  up (7.2). Browsing plainly and matching closely are the same mechanism at
  different levels of detail.
- **Decided:** shared links and crawlers get the game's feed with no description:
  every post, ordered by activity. A description is personal and isn't shared.
- **Decided:** the description is a post draft. Publishing it opens the post screen
  prefilled from it (section 6, step 3), and the feed invites it: "Publish
  this as your post: groups and players can find you too, and we'll tell you when
  someone new fits."
- **Proposed:** the draft is stored locally, so seeing what fits needs no account.
  Publishing does.
- **Proposed:** a player with a post in the game gets the description prefilled
  from it: from their player post if they have one, otherwise from their group or
  community post. Changing the description offers to update the post. That
  description isn't empty, so their own post leaves the feed (section 4).
- **Proposed:** the bar carries only the fields matching compares for the chosen
  type (7.2), the most used first and the rest under **More**. Fields that aren't
  compared, such as a group's size or name, are filled in on the post screen after
  **Publish post**.
- **Decided:** on a phone the bar is a full-screen modal. The feed updates when the
  modal is closed, not as each field is filled in.

### 7.2 What fits

**Decided:** matching is fuzzy. Nothing is ruled out: posts that match every field
come first, then posts matching progressively fewer. Each card says what fits and
what doesn't (section 5.6), so the ordering is legible.

```
Fits you (3)
  group card      ✓ Needs Controller · ✓ Diamond fits · ✓ EU · ✓ 21–23
  player card     ✓ Diamond 1 · ✓ EU · ✓ 19–22 · ✓ EN
  community card  ✓ EU · ✓ EN · ✓ PC
Missing one thing (12)
  group card      ✓ Needs Sentinel · ✓ EU · ✓ 21–23 · ≠ Platinum 1–2
Missing more
  player card     ✓ EU · ≠ Bronze 2 · ≠ Online 08–12 · ≠ PT
```

**Decided:** posts are split into tiers by how many fields they match, each under
its own heading, so the ordering explains itself.

**Open:** what makes a post a fit. Some fields must match for a post to fit;
others are nice to have and only lift it within the feed. A fit is what the "Fits
you" tier, the Matches screen (section 6) and match notifications (section 8)
count. Which fields must match, whether the system or the player decides, and how
a field left empty on either side counts are settled in testing and tuned after
launch.

**Proposed:** what a field compares against depends on the two post types, not on
the field:

| Viewer is a | Shown       | Fields compared                                                                 |
| ----------- | ----------- | -------------------------------------------------------------------------------- |
| Player      | Groups      | The viewer's roles against the group's needed roles; their rank inside its range; their location against its regions; language, platform, age, hours, microphone, Looking for |
| Player      | Players     | Rank closeness; location, language, platform, age, hours, microphone, Looking for; shared roles count as a match |
| Player      | Communities | Location against its regions; language, platform, Looking for                                    |
| Group       | Players     | The group's needed roles against the player's roles; the group's rank range against their rank; the rest as above |
| Community   | Players     | Regions against their location; language, platform, Looking for                                    |

- **Proposed:** hours overlap in the viewer's timezone, and "near" rank is a few
  steps either way in the game's ordered rank options.
- **Decided:** a player's location is compared through the region it maps to:
  against a group's or community's regions, or against another player's region.
  Game fields such as server region compare directly.
- **Decided:** platform and Looking for exist only where the game has them, with
  options set per game (section 5).
- **Decided:** every field counts the same when splitting posts into tiers.
  Weighting fields by how much they matter is out of scope until there is usage
  data to base it on.
- **Decided:** nothing is ever excluded from the feed, so a Portuguese-speaking
  player still sees English-only posts, marked `≠ EN`, below the ones that fit.
  With supply this thin that is worth more than a shorter, more correct page.
- **Proposed:** fields that don't apply between two types, such as rank against a
  community, count neither for nor against.
- **Decided:** the active and expired divider is the outer split: an expired post
  never outranks an active one. The tier headings sort the active posts above it;
  below it, expired posts follow the same order without headings.
- **Proposed:** a thin page is now honest by construction. Rather than an empty
  result, the viewer sees weak matches labelled with what doesn't fit, followed by
  the prompt to publish: "Publish your post and we'll tell you when someone fits."

## 8. Match notifications

- **Decided:** there are no separate alerts. Every post notifies its owner when a
  post that fits it appears (7.2): a player post of new groups, communities and
  players; a group or community post of new players.
- **Decided:** notifications need an account, since publishing does.
- **Decided:** notifications go by email and to an on-site notification badge.
- **Proposed:** a notification fires when a fitting post is published, or renewed
  after it expired. Edits and renewals of active posts don't notify.
- **Decided:** every fitting post is its own notification; notifications are never
  merged into one. An email can carry several, grouped by the post of yours they
  fit.
- **Proposed:** notifications stop when the post expires, and resume when it is
  renewed.
- **Decided:** each post in a match email links to that post's page (11.1).

## 9. Freshness

- **Decided:** a post is active for 30 days after it is published, renewed or edited.
  Before it expires, a renewal email renews it in one click, without signing in.
- **Decided:** the renewal link opens the game's feed with the description taken
  from the renewed post, under a note that it is active again. The link works
  signed out, so it names the post the description comes from.
- **Decided:** community posts are active for 90 days.
- **Decided:** expired posts stay in the feed below the divider (section 4).
- **Proposed:** an expired post can be renewed any time from the home page (11.2).
- **Proposed:** expired posts can still be messaged, with a note that the owner may
  not reply. The owner's email about the message includes a **Renew** button.
- **Proposed:** the card shows "Active 2 days ago" for active posts and "Expired
  2 months ago" for expired ones, both counted from the last renewal or edit.

## 10. Messaging

- **Decided:** every conversation is between a signed-in player and a post's owner,
  about that post. Players can't message each other outside a post.
- **Decided:** a player and the other player's post have one conversation. If two
  players each message the other's post, those are two conversations.
- **Decided:** no rate limits. Abuse is handled if it appears.
- **Decided:** blocking and reporting exist from launch. Reports are stored and
  emailed to the site admin.
- **Decided:** a block hides the two players from each other, both ways. Neither
  sees the other's posts in a feed, their conversations leave both inboxes, and
  neither is notified of the other's posts. Unblocking brings it all back, so
  nothing is deleted.
- **Decided:** a post page is public, so a blocked player can still reach one
  through a link. The page shows the post without its contact button.
- **Decided:** an email is sent when a conversation receives a message while it has
  no unread messages for the recipient. Further messages in a conversation that
  already has unread ones send nothing. This is counted per conversation.
- **Decided:** a message email opens the conversation in the inbox.
- **Decided:** no Discord integration besides sign-in.
- **Proposed:** either side can start a conversation, through the other's post. A
  player without posts can message.
- **Decided:** deleting a post deletes its conversations, for both sides. Before
  deleting, the owner is told how many: "3 conversations will be deleted for both
  of you." A post left to expire keeps them.
- **Decided:** deleting an account deletes everything tied to it: its posts, and
  with them their conversations, and the conversations it started on other posts.
- **Out of scope:** reply rate, and ranking or badges derived from it.

### Inbox

**Proposed:** the inbox is grouped by post, which tells owners what their posts
produced.

```
┌──────────────────────────────────────────────────────────────────────┐
│ Messages                                                             │
│                                                                      │
│ Your posts                                                           │
│   Night Owls · Valorant group        3 conversations  ● 1 new        │
│   Kestrel · Dota 2 player            1 conversation                  │
│                                                                      │
│ Posts you messaged                                                   │
│   The Farlands · Valheim community   Eirik: "Welcome, here's the…"   │
│   ShadowFox · Valorant player        You: "Want to duo tonight?"     │
└──────────────────────────────────────────────────────────────────────┘
```

```
┌──────────────────────────────────────────────────────────────────────┐
│ ← Night Owls · Valorant group                                        │
│   Conversation with Vex · Diamond 1 · Controller · EU                │
│ ──────────────────────────────────────────────────────────────────── │
│   Vex: Hey, saw you need a Controller, I main Omen…                  │
│   You: Nice, we play around 9 CET, want to join tonight?             │
│ ──────────────────────────────────────────────────────────────────── │
│ [ Write a message…                                        ] [ Send ] │
└──────────────────────────────────────────────────────────────────────┘
```

**Proposed:** the conversation header shows the other player's card facts when they
have a post in the same game, and their nickname otherwise.

## 11. Around the feed

### 11.1 Post pages

- **Decided:** a post's own page is the post as an expanded card and a way into
  the game's feed, which shows the viewer what fits their own description.
- **Decided:** Back from a post page returns to the feed with its loaded batches
  intact.
- **Decided:** an expired post's page is kept out of search engines. It carries a
  robots `noindex` tag while the post is expired, which crawlers get in the
  prerendered HTML, and the sitemap leaves the post out. Renewal removes both.
- **Proposed:** a deleted post's page says the post is gone and links to the feed,
  and answers crawlers with a 404.
- **Decided:** there is no public player page. A post's own page is the only
  public page about a player.

### 11.2 Home page

- **Decided:** the home page starts post creation. It opens with "What are you
  posting?" (section 6, step 1), and the game cover grid below it leads to each
  game's feed.
- **Decided:** a signed-in player's home page is their posts. Games they have
  posts in come first, each with its cover and their posts beside it. Each post
  shows its state, conversations and contact reveals, with **See what fits**,
  **Edit** and **Renew**; **See what fits** opens the feed with the description
  taken from the post. The other games follow as the plain cover grid, and
  **New post** starts the flow at the type step.
- **Decided:** a signed-in player with no posts gets the signed-out home page.
- **Decided:** the rest of the account is the account page (11.5), reached from
  the account menu in the header (11.4).

### 11.3 Notification list

- **Decided:** notifications are a dropdown list in the header, and each opens
  what it is about:

| Notification                          | Opens                         |
| ------------------------------------- | ----------------------------- |
| A new post fits one of yours (8)      | That post's page (11.1)       |
| One of your posts is about to expire  | The home page (11.2)          |

- **Proposed:** the list is grouped by the player's own posts, each notification
  under the post it is about, so a player with two posts in a game can tell them
  apart.
- **Decided:** every fitting post is its own notification, never merged with
  others (section 8).
- **Decided:** messages aren't notifications. The inbox's link in the header shows
  how many conversations are unread.
- **Proposed:** contact reveals are counts on the home page, never notifications.

```
┌──────────────────────────────────────────┐
│ Notifications              Mark all read │
│                                          │
│ Night Owls · Valorant group              │
│ ● Vex fits · player · 2 hours ago        │
│ ● Mira fits · player · yesterday         │
│                                          │
│ Kestrel · Dota 2 player                  │
│ ● Expires in 3 days · 2 days ago         │
│   Ancients fits · community · last week  │
└──────────────────────────────────────────┘
```

### 11.4 Header

```
Signed out
┌──────────────────────────────────────────────────────────────────────────┐
│ ◆ TeamTavern   Games ▾                  [ New post ]   Sign in   Sign up │
└──────────────────────────────────────────────────────────────────────────┘
Signed in
┌──────────────────────────────────────────────────────────────────────────┐
│ ◆ TeamTavern   Games ▾                  [ New post ]   ✉ 2   🔔 3   K ▾  │
└──────────────────────────────────────────────────────────────────────────┘

Phone, signed out                          Phone, signed in
┌────────────────────────────────────────┐ ┌────────────────────────────────────────┐
│ ◆  Games ▾              [ + Post ]  ☰  │ │ ◆  Games ▾         [ + ]  ✉ 2  🔔 3  K │
└────────────────────────────────────────┘ └────────────────────────────────────────┘

Account menu
┌──────────────────────┐
│ Kestrel              │
│ Your posts           │
│ Account              │
│ ──────────────────── │
│ Sign out             │
└──────────────────────┘
```

- **Proposed:** the logo leads home: the start of post creation signed out, the
  player's posts signed in (11.2).
- **Proposed:** **Games** opens the cover grid, and each cover opens that game's
  feed. For a signed-in player, games where they have a post carry a mark.
- **Proposed:** **New post** is the header's one prominent button, signed in or
  out, since it is the start of the funnel. **Sign up** is a plain link: most
  players register through New post, and the rest are asked to when they first
  open a contact panel.
- **Proposed:** the inbox icon opens the inbox page and shows how many
  conversations are unread. The bell opens the notification list (11.3).
- **Proposed:** the account menu holds **Your posts**, **Account** and **Sign
  out**. Your posts repeats the logo's destination, since nobody expects the
  logo to lead to their own posts.
- **Proposed:** on a phone the logo shrinks to its mark, and New post stays
  visible. Signed out, a menu holds Sign in and Sign up. Games and notifications
  open full-screen, like the description bar, and the account menu opens as a
  sheet.

### 11.5 Account page

```
┌───────────────────────────────────────────────────────────┐
│ Account                                                   │
│                                                           │
│ Shown on your posts                                       │
│   Nickname    Kestrel                                     │
│   Birthday    12 April 1998, shown as age 28              │
│   Location    Croatia                                     │
│   Languages   Croatian, English                           │
│   Timezone    Europe/Zagreb                               │
│   Contacts    Discord kestrel · Riot ID Kestrel#EUW       │
│                                                  [ Edit ] │
│                                                           │
│ Only you see this                                         │
│   Sign-in     kestrel@example.com, password   [ Change ]  │
│   Emails      [✓] Matches  [✓] Messages  [✓] Renewals     │
│   Blocked     2 players                       [ Manage ]  │
│                                        [ Delete account ] │
└───────────────────────────────────────────────────────────┘
```

- **Decided:** one account page with two sections. **Shown on your posts** holds
  the facts about the player and their contacts, which posts show live (section 6,
  step 3), so editing them changes every post at once. **Only you see this**
  holds how the player signs in, email switches, blocked players and deleting the
  account.
- **Proposed:** sign-in shows the player's one way of signing in, an email and
  password or Discord, and changes it.
- **Proposed:** match, message and renewal emails each have an on/off switch.
  Every email carries an unsubscribe link, and it lands on these switches.
- **Proposed:** blocked players are listed with **Unblock**, which brings back
  what the block hid (section 10).
- **Decided:** **Delete account** deletes the account's posts and conversations
  (section 10), and says so, with the counts, before it does.

## 12. Relaunch and measurement

- **Decided:** the redesign is a relaunch. Data that doesn't fit the new model may be
  dropped.
- **Proposed:** what today's rows become, subject to the model:

| Today                              | Becomes                                      |
| ---------------------------------- | -------------------------------------------- |
| Player profile                     | Player post, expired unless recently updated |
| Team profile, size party           | Group post, owned by the team's owner        |
| Team profile, size community       | Community post                               |
| Team with no profile (1,200)       | Nothing                                      |
| Team name, website, Discord server | Fields on the group or community post        |
| About and ambitions                | Joined into the one text field               |
| Alert                              | Nothing, or a post for its owner (open)      |

- **Open:** existing alerts come from anonymous emails and have no place in the new
  model. Whether any carry over is decided with the model; dropping them all is
  acceptable.
- **Open:** a one-time relaunch email to existing players and alert subscribers.
- **Open:** Search Console shows few paginated feed pages were ever indexed. Watch
  indexing of posts and feeds after launch.
- **Proposed:** a minimal first-party event log from launch, so the relaunch can be
  judged: feed views, descriptions started, posts published, post views, contact
  reveals, conversations started, renewals and returning visits.

## 13. Data model gaps

The model is redesigned from this brief, not before it. What the brief needs that
the current model lacks:

- **Posts.** Player, group and community posts in place of player profiles, teams,
  team profiles and alerts, with at most one post per player, type and game.
- **Group fields.** Size, open slots, needed roles, a rank range, an age range, a
  set of regions, and an optional name, website and Discord server.
- **Community fields.** Name, experience level, Discord invite and website.
- **One free text field** in place of about and ambitions, and a **Looking for**
  value on all three post types.
- **Game fields.** Which fields appear on a card, whether a field takes one option
  or several, and platform, a community's kind and the Looking for options as
  per-game fields with per-game options.
- **Regions.** Group and community regions, and a mapping from every location to
  a region.
- **Freshness.** A last renewal or edit time that sorts the feed and decides expiry,
  30 days for player and group posts and 90 for communities.
- **Renewal links** that renew one post without signing in.
- **One feed query** across all three post types, sorted and batched together.
- **Matching.** How many fields of a post match a description or another post
  (7.2), per pair of post types, including rank closeness from the game's ordered
  rank options; usable as a sort key and as a cursor for Load more, and degrading
  to plain recency when the description is empty. Which fields must match for a
  post to fit.
- **Match notifications.** One per fitting post, fired on publish and on renewal
  after expiry, for the owners of posts it fits, with emails that carry several.
- **Messaging.** Conversations tied to a post, messages, unread state, blocks and
  reports.
- **Notifications.** An on-site notification list and badge.
- **Contact preference** on posts, and a count of contact reveals per post.
- **Email switches** on the account, one per kind of email.
- **Events** for measurement.

## 14. Not covered yet

- Competitions and leagues.
- Design system and visual language.
- Ads. Every layout in this brief is ad-free. Ads fit around the design rather
  than the design around them, so they are placed last. Side panels and a bottom
  sticky are likely; the rest is open.

## 15. Out of scope

Decided against for now, listed so they aren't reopened by accident:

- **Reply rate**, and any ranking or badges derived from it.
- **Discord integration beyond sign-in**: no bot, no DMs, no invite validation.
- **Rate limits** on messaging or posting, until abuse appears.
- **Per-field weighting** in matching, until there is usage data.
- **Excluding posts from the feed**: fields that must match decide what fits
  (7.2), but nothing is ever hidden.
- **Per-type notification settings**, and a player field for "not interested in
  communities".
- **Competitions and leagues**: wanted, but not in the first release.
- **Community logos**: uploads and moderation.
- **Newest posts on the home page**: wanted, but not in the first release.
- **Members, shared inboxes and roles** on groups and communities: they are posts,
  not entities.
- **Closing a post that worked**: a group that fills its last slot deletes the post,
  which deletes its conversations after a warning, or lets it expire, which keeps
  them. Whether a "We found everyone" action earns its place, one that takes the
  post out of the feed and keeps its conversations, is decided from usage after
  launch.
