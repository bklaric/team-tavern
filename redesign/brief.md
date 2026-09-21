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
│ Tell us about you                                                       │
│ (•) I'm a player looking for a group                                    │
│ ( ) We're a group looking for players                                   │
│ ( ) We're a community looking for members                               │
│                                                                         │
│ Diamond 2 ▾  Controller, Sentinel ▾  Croatia ▾  EN, DE ▾  19:00–23:00 ▾│
│ More ▾                                                                  │
├─────────────────────────────────────────────────────────────────────────┤
│ ✦ Publish this as your post: groups and players can find you too, and   │
│   we'll tell you when someone new fits.               [ Publish post ]  │
├─────────────────────────────────────────────────────────────────────────┤
│ Showing  [ All ]  [ Players ]  [ Groups ]  [ Communities ]              │
│                                                                         │
│ Fits you (3)                                                            │
│  group card      ✓ Controller · ✓ Diamond · ✓ EU · ✓ 21:00–23:00        │
│  community card  ✓ EU · ✓ EN · ✓ PC                                     │
│  player card     ✓ Diamond 1 · ✓ EU · ✓ 19:00–22:00 · ✓ EN              │
│ Missing one thing (12)                                                  │
│  group card      ✓ Sentinel · ✓ EU · ✓ 21:00–23:00 · ≠ Plat 1–2         │
│ Missing more                                                            │
│  player card     ✓ EU · ≠ Bronze 2 · ≠ 08:00–12:00 · ≠ PT               │
├──────────── Older posts · they may no longer be looking ────────────────┤
│  player card (dimmed)                        Active 3 months ago        │
│  group card (dimmed)                         Active 1 year ago          │
│                           [ Load more ]                                 │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Decided:** the feed has no filters to set. The viewer says whether they are a
  player, a group or a community, and describes themselves with the same fields
  their post would have. The feed shows the posts that fit (section 7). The
  description is a post draft, so publishing it is a few clicks away.
- **Decided:** a player viewer sees groups, communities and players, and narrows
  what is shown with **Showing: All, Players, Groups, Communities**. That changes
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

**Decided:** platform and the Looking for options (5.5) are game fields: a game
has them only where they make sense, with options of its own. There is no field
for what kind of thing a community is: what it runs on and how it is played are
already the game's own fields, such as Valheim's server type, and a community
that is a clan or a Discord says so in its words. Each field takes one option or several, as suits
the field. A game field such as server region is the same field on all three
post types.

**Proposed:** platform's options are what a player plays on, not where they bought
the game: PC, PlayStation, Xbox, Switch. A post picks every platform it plays on,
all of them for a crossplay community. A game on one platform has no platform
field. Where a game has one, it is on all three cards, after the languages.

**Decided:** a player gives their location, which is a country and only a
country; a group or community gives regions, and only regions. Every country
belongs to exactly one region, which is how the two are compared (7.2), so it is
one comparison whichever pair of post types is being matched.

**Decided:** a region is the set of places close enough to each other to play
together, which is what the latency between them decides. Whether two players
can understand each other is the languages field's business, so a split that is
really about language does not belong here: West and East Europe are one region,
because Lisbon to Warsaw is about 50ms and what separates those players is that
they speak different languages. The twelve:

| | | |
| ---------------- | ------------------ | ---------------- |
| Europe           | North America      | South Asia       |
| Middle East      | Central America    | East Asia        |
| North Africa     | South America      | Southeast Asia   |
| Sub-Saharan Africa | Central Asia     | Oceania          |

**Decided:** what puts a country in a region is where its players are served,
not the continent a map draws it on. Two players never ping each other: both
ping a game server, so a region holds the countries whose players can find a
common server playable for both, and nine of the twelve are a region some
publisher already runs servers for. Russia is in Europe, since most of its
players are west of the Urals and play on European servers, and Turkey, the
Caucasus and Cyprus are too, since Istanbul is about 45ms from Frankfurt and
every publisher routes them west. Where the map and the servers disagree, the
servers decide:

| Country       | Region          | Why                                                                                            |
| ------------- | --------------- | ---------------------------------------------------------------------------------------------- |
| Greenland     | Europe          | Its cable runs through Iceland to Denmark, and nothing it reaches is in North America          |
| Sudan         | Middle East     | Its cables land in Jeddah, 30ms off, where Khartoum to Casablanca is over 100ms through Europe |
| Mexico        | North America   | Monterrey is 25ms from Dallas, Tijuana 110ms from Panama City                                  |
| The Caribbean | Central America | Havana is 40ms from Mexico City and 100ms from Seattle, and Florida only makes it look near    |

**Decided:** the twelve stay, though latency alone would give about six. Dallas
is about 45ms from both San Juan and Seattle, and São Paulo is nearer Bogotá than
Lisbon is Moscow, so the Americas would be one region and East and Southeast Asia
another. A region also has to be legible to the player picking one, and it has to
filter: one that holds everyone filters nothing. The splits past the sixth are
what keep the field worth answering.

**Decided:** North Africa, Central Asia and Sub-Saharan Africa have no servers of
their own, and are kept anyway. The Maghreb plays European servers and Egypt
Middle Eastern ones; Central Asia plays Moscow's, and Almaty is 110ms from
Frankfurt, too far to fold into Europe; Sub-Saharan Africa has Johannesburg,
which Lagos is 120ms from. Each holds players nobody serves well, which is a
reason to name the region rather than to merge it away.

**Decided:** the mapping is every country and inhabited dependency of more than
about ten thousand residents, some 230 of them, each under the name a player
would look for rather than its official one.

**Open:** countries on the edge of a region play happily with the region next
door. Sorting a post that misses by the nearest region, rather than counting it
a plain miss, is worth doing and is out of scope for the first release.

**Decided:** online hours wait behind Details (5.4), like a player's age, and
join the end of the fact line while the viewer's description compares them. They
show in the viewer's timezone, converted from the owner's, so they read the same
way as the ✓ and ≠ marks (5.6). They follow the viewer's locale, "19:00–01:00" or
"7pm–1am", and carry no timezone name: the viewer knows their own.

**Decided:** a player post's facts say what the player is; a group's or a
community's say what it is looking for. The same field reads both ways: a group's
rank range is the range it wants, its regions are where it wants players, its
microphone is one it wants them to have. Nothing on the card marks the
difference, because the post's type already does, and the description bar says
the same thing as the viewer chooses (7.1).

**Decided:** a card has no badges. Looking for is a fact like any other, in the
fact line or behind Details (5.4). The fact line wraps as it needs to.

### 5.1 Player card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ ShadowFox                                  PLAYER    Active 2 days ago  │
│ Diamond 2 · Duelist, Initiator · Croatia · EN, DE · 🎤 ·                │
│ Ranked climb                                                            │
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together…"                            │
│                                  [ Add on Discord ]        Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

### 5.2 Group card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Night Owls   GROUP  3 players, wants 2 more          Active 5 hours ago │
│ Platinum 1 – Diamond 3 · Controller, Sentinel · EU · EN ·               │
│ 🎤 · Ages 18+ · Ranked climb                                            │
│ "Three friends who play most nights, we want to stop solo queuing for   │
│  the last two spots. No tilt, comms on…"                                │
│ Posted by Kestrel                       [ Message ]        Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** the heading says how many the group is and how many more it wants,
  "3 players, wants 2 more", or "wants 2–3 more" where either will do. The needed
  roles are the second fact, where a player card has its roles, so player and group
  cards read the same way.
- **Proposed:** ranges and sets replace single values: rank range, age range, one
  or more regions.
- **Proposed:** a name is optional; without one the heading reads "Kestrel's group".
- **Proposed:** "Posted by" names the owner, since conversations are between players.
  It is plain text: there is no player page to link to (11.1).
- **Proposed:** the website sits behind the contact button (5.6).

### 5.3 Community card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ The Farlands   COMMUNITY                              Active 1 week ago │
│ Modded · EU · EN · PC · Events, Casual                                  │
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
- **Proposed:** a community carries a Looking for (5.5) in the fact line with its
  other fields. Every field value stays in that line whatever the post type, so
  nothing has to decide which belong in the heading.
- **Proposed:** a community carries an age range, a microphone and online hours
  as a group does, and they are compared with a player's the same way (7.2).
  Production fills them on 88%, 83% and 47% of its community profiles, so they
  are facts communities do give.
- **Out of scope:** community logos. They need uploads and moderation.

### 5.4 Expanding a card

```
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together. I play most evenings after  │
│  work and I'm free all weekend."                                        │
│                                                                         │
│ Age 24 · Usually online 19:00–23:00 · Agents: Jett, Raze, Sova          │
│ Playstyle: Aggressive · Tracker: tracker.gg/shadowfox ↗                 │
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
| Player    | Rank, roles, location, language, platform, microphone, Looking for | Age, online hours, remaining game fields, trackers        |
| Group     | Open slots, needed roles, rank range, regions, language, platform, microphone, ages | Online hours, remaining game fields     |
| Community | Regions, language, platform, microphone, ages, Looking for         | Online hours, remaining game fields, the rest of the pitch                |

### 5.5 Free text and "Looking for"

- **Proposed:** about and ambitions merge into one field, "About you and what you're
  looking for". Ambitions are left empty most often, overlap with about when both
  are written, and are nearly always one of a few answers.
- **Proposed:** a structured **Looking for** on all three post types, one or more
  of the game's options, such as Casual, Ranked climb or Competitive for Valorant,
  and Events or Roleplay for Valheim. The choices are one fact on the card and one
  matching field.
- **Decided:** the options are set per game and are the same for all three post
  types, so a player's answer compares directly with a group's or a community's.

### 5.6 Card actions

- **Proposed:** in the feed, each card says what fits the viewer's description and
  what doesn't, which is what makes the fuzzy ordering (7.2) legible.
- **Decided:** the marks sit on the card's own facts: "✓ Controller",
  "✓ Platinum 1 – Diamond 3", "≠ PT". A separate row of marks would repeat most of
  the fact line. A field the viewer filled in and the post left empty shows in the
  place it would take, "≠ Rank not given" where the rank would be. A compared
  field the card doesn't otherwise show, such as a player's age behind Details,
  joins the end of the fact line with its mark.
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
- **Decided:** on a desktop the panel is a side panel on the right, not a modal,
  so the card it was opened from stays in view beside it; on a phone it is
  full-screen.
- **Proposed:** the preferences that point off-site (Add on Discord, Add in game,
  Join Discord, Visit site) put the contacts first, under a heading in the
  owner's terms ("Prefers Discord", "Join on Discord"), and "or message on
  TeamTavern" follows. Message me and Either is fine put the message box first,
  and "or add Kestrel off-site" follows. A conversation already under way
  always comes first.
- **Proposed:** the panel's one filled button is what comes first: **Send**, or a
  community's **Open the invite** or **Visit site**. When contacts come first,
  **Send** is outlined.
- **Proposed:** before the first message, the panel says where replies go: "Your
  message starts a conversation about Night Owls. Replies show up here and in
  your inbox." Messages carry no subject or greeting template.
- **Proposed:** Enter sends and Shift+Enter starts a new line, as on Discord. On
  a phone Enter is a new line and **Send** sends.
- **Proposed:** the panel's menu holds **Report this post** and **Block Kestrel**
  (section 10).

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
│ Group name       [Night Owls]        │ │ Night Owls GROUP             │ │
│ How many are you, and how many more  │ │ 3 players, wants 2 more      │ │
│ do you want?                         │ │ Plat 1 – Dia 3 · Controller, │ │
│   [ 3 ] players, want [ 2 ] more     │ │ Sentinel · EU · EN · 🎤 ·    │ │
│ Roles you need   [Controller][Sent…] │ │ Ranked climb                 │ │
│ Rank range       [Plat 1] – [Dia 3]  │ │                              │ │
│ Regions          [EU ▾]              │ │ "Three friends who play most │ │
│ Language         [EN ▾]              │ │  nights, we want to stop…"   │ │
│ Ages             [18] – [  ]         │ │ Posted by Kestrel [ Message ]│ │
│ Looking for      [✓] Ranked climb    │ │                     Details ⌄│ │
│ Microphone       [✓] Required        │ └──────────────────────────────┘ │
│ Usually online   [21:00] – [01:00]   │                                  │
│ Timezone         Europe/Zagreb       │                                  │
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
│ Your post stays active for 30 days.  │                                  │
│ We'll email you before it expires,   │                                  │
│ and tell you when someone new fits.  │                                  │
│                     [ Publish post ] │                                  │
└──────────────────────────────────────┴──────────────────────────────────┘
```

   The screen has three parts:

   - **The post's fields.** Every field of the post, the game's own fields among
     them, with nothing behind a click.
     - *Player:* rank, roles, the game's other fields, location, language,
       birthday, microphone, Looking for, online hours and timezone.
     - *Group:* optional name, how many you are and how many more you want,
       needed roles, rank range, the game's other fields, regions, language,
       microphone, age range, Looking for, online hours and timezone.
     - *Community:* name, Looking for, regions, language, platform, age
       range, microphone, online hours and the game's fields.
   - **In your words.** One text field with prompts and an example. **Decided:**
     required for communities, where the text is the product, and optional for
     player and group posts.
   - **Contact.** How people reach the post, prefilled from the account.

   - **Decided:** online hours are one range, "Usually online 21:00–01:00", in the
     player's timezone, and a range may cross midnight. There are no separate
     weekday and weekend hours: online times are the field players leave empty most
     often (56% filled) and the criterion alerts use least (41%), and a second range
     doubles that friction.
   - **Decided:** what the card shows up front and what it keeps behind Details
     (5.4) is the card's concern, not the screen's: the screen asks for every field
     in one place. A field behind a click is a field left empty, and online hours
     are already the one players skip most.
   - **Decided:** only a community's name and its words are required. They carry a
     **Required** tag, and no field carries an optional one. The game account ID
     (Riot ID, Steam ID and the like) is optional like every other contact:
     messaging on the site reaches every post.
   - **Decided:** trackers aren't fields. Each game's tracker templates build them
     from the player's game account ID, and the ID's field says which profiles the
     card will link.
   - **Decided:** facts about the player (age, languages, location, timezone) and
     their contacts are asked once and live on the account. This screen asks for
     them where the account doesn't have them yet, with the post's fields. There is
     no separate details step. Player posts show the facts
     as the account has them now, and every player and group post the player owns
     shows their contacts; changing either on the account page (11.5) doesn't renew
     any post. Group and community posts carry their own regions, languages and age
     range: regions prefilled from the region of the owner's location, languages
     from the account.
   - **Decided:** a player who already has other posts is told, beside an account
     fact or contact they change here, that the change applies to all their posts.
     A player writing their first post isn't.
   - **Proposed:** an account fact or contact the account already holds shows as
     its value, "Croatia · From your account · Change", not as an input, and
     Change opens the input.
   - **Decided:** the preview expands with **Details ⌄**, as cards do in the feed
     (5.4). Most fields change the card itself; some, such as online hours, fill in
     only what the expansion shows.
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
   - **Decided:** below a desktop the preview is opened on demand: a bar at the
     bottom of the screen holds **Preview** and **Publish post**, and Preview
     opens the card in a sheet. A card above the fields scrolls away before the
     player reaches the text they write, which is what the preview is for.
4. **Register or sign in** (signed out only). The draft is kept through it.
   - **Decided:** this is the site's one sign-up screen: email, nickname and
     password, or **Continue with Discord**, with a link to sign in instead. The
     header's **Sign up**, and opening a contact panel signed out, lead to the same
     screen, and it returns the player to where they were.
   - **Decided:** registration always asks for a nickname. A player who signed up
     with Discord on the post screen sees only the nickname prompt here; one who
     continues with Discord here goes on to the same prompt. Discord prefills it,
     and the player can edit it.
   - **Proposed:** registering with Discord fills the Discord contact with the
     username and the email with the address Discord gives, both of which the
     player may then change. Signing in with Discord fills an email the account
     lacks and never replaces one, since the player may have chosen another.
   - **Proposed:** an address is confirmed either by Discord, which says whether
     it verified it, or by a link the site emails: a typed address, an address
     Discord has not verified, and a changed one all get the link. Until it is
     clicked, that link is the only email the site sends to the address. Nothing
     else waits on it: posting, contacting and messaging are the site's own.
   - **Proposed:** a Discord account may arrive with no address at all, such as
     one registered on Discord with a phone number. The player posts and messages
     as anyone does, and the account page asks for an address so the site can
     write.
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
│ [ See all ]                                               │
└───────────────────────────────────────────────────────────┘
```

   - **Proposed:** with nothing matching well, the closest posts are shown with
     what doesn't fit, and "We'll email you when someone fits."

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
- **Proposed:** a viewer with a post of the chosen type in the game is never asked
  to publish another. While the description says what that post says, the
  publish prompt's place holds a muted line with no button: "Showing what fits
  Night Owls, your group post." Once it differs, the prompt reads "Update Night
  Owls with this" and its button **Update post** opens the post screen on the
  post, with the description's fields in place of the post's.
- **Proposed:** the bar is headed "Tell us about you", with a line saying that what
  fits comes first. Without it the chips read as ordinary filters, not as the
  viewer's own details.
- **Proposed:** under the three choices a line says which way to read the fields,
  since it changes with the choice: a player describes themselves, a group or a
  community describes the players it wants. It is the same inversion the cards
  carry (5.2), and the bar is where the viewer first meets it.
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
  group card      ✓ Controller · ✓ Diamond · ✓ EU · ✓ 21:00–23:00
  player card     ✓ Diamond 1 · ✓ EU · ✓ 19:00–22:00 · ✓ EN
  community card  ✓ EU · ✓ EN · ✓ PC
Missing one thing (12)
  group card      ✓ Sentinel · ✓ EU · ✓ 21:00–23:00 · ≠ Platinum 1–2
Missing more
  player card     ✓ EU · ≠ Bronze 2 · ≠ 08:00–12:00 · ≠ PT
```

**Decided:** posts are split into tiers by how many fields they match, each under
its own heading, so the ordering explains itself.

**Decided:** a field the viewer filled in and the post left empty counts as a
mismatch. A post that says nothing about rank doesn't fit a Silver player any
better than one that says Diamond, so a sparse post can't rise above posts that
give their details. A field the viewer left empty isn't compared at all.

**Proposed:** the tiers count against the fields that apply to the post's type:
"Fits you" matches every one of them, "Missing one thing" all but one. A post with
none of them, such as a community when the viewer has filled in only rank and
roles, goes to the last tier.

**Decided:** every field the viewer filled in must match for a post to fit. A fit
is what the "Fits you" tier, the Matches screen (section 6) and match
notifications (section 8) count, and it is the top tier by another name: a post
fits when it misses nothing the viewer asked about. The viewer decides how tight
that is by how much they fill in, which is the same control the description
already gives them, so nothing else has to be tuned.

**Decided:** a description that fits nothing produces no notification rather than
a loose one. Telling an owner about the closest posts when none fit is worth
doing and is out of scope for the first release: it is a second kind of
notification with its own copy and its own threshold, and it is easier to judge
once there is traffic to judge it on.

**Proposed:** what a field compares against depends on the two post types, not on
the field:

| Viewer is a | Shown       | Fields compared                                                                 |
| ----------- | ----------- | -------------------------------------------------------------------------------- |
| Player      | Groups      | The viewer's roles against the group's needed roles; their rank inside its range; their location against its regions; language, platform, age, hours, microphone, Looking for |
| Player      | Players     | Rank closeness; location, language, platform, age, hours, microphone, Looking for; the two of you covering two different roles |
| Player      | Communities | Location against its regions; language, platform, age, hours, microphone, Looking for            |
| Group       | Players     | The group's needed roles against the player's roles; the group's rank range against their rank; the rest as above |
| Community   | Players     | Regions against their location; language, platform, age, hours, microphone, Looking for            |

- **Proposed:** hours overlap in the viewer's timezone, and "near" rank is a few
  steps either way in the game's ordered rank options.
- **Proposed:** between two players, roles fit when the two of you can cover two
  different roles, which is all a duo needs, so the only miss is two players who
  each play one role and it is the same one. Two Mid mains can't duo, a Mid
  looking for a Jungler is the ordinary case, and two players who both play Mid
  and Jungle fit by splitting them. Roles are the one field where what you are
  short of is what you are looking for, so fitting is covering different options
  rather than holding the same one. Both of you get the same answer, where a
  rule that read one set against the other would call a pair a fit from one seat
  and a miss from the other. Between a player and a group nothing changes: a
  group names the roles it needs and you fit by filling one.
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
- **Decided:** the emails go out on a period rather than on the instant. A worker
  takes the fits and expiries of the period that has passed and sends each owner
  one email, grouped by the post of theirs they are about. Nothing records what
  has been sent, since a notification falls in one period: a post renewed after
  it expired fits again, and that puts its notification in the period the
  renewal falls in. An expiry the owner has already renewed away is gone before
  the worker sees it, so nothing is sent.
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
- **Decided:** every card says when it was last active, "Active 2 days ago" or
  "Active 3 months ago", counted from the last renewal or edit. Players don't know
  when a post expires, so "Expired 2 months ago" would read a month off; the divider
  and the faded card say that it has.

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
  neither is notified of the other's posts. Unblocking, from the blocked list on
  the account page (11.5), brings it all back, so nothing is deleted.
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
produced. Under each of the player's own posts are its conversations, one row
per player who wrote, so an owner sees who wrote without opening the post
first. A post of theirs that has expired says so and offers **Renew**.

```
┌──────────────────────────────────────────────────────────────────────┐
│ Messages                                                             │
│                                                                      │
│ Your posts                                                           │
│ ▌ Night Owls · Valorant group · 3 conversations                      │
│     Ashen                                          10 hours ago    ● │
│     Hi, Diamond support here, mostly Killjoy and Cypher…             │
│     Vex                                              2 days ago      │
│     You: Sent. See you at 9                                          │
│ ▌ Kestrel · Dota 2 player · 1 conversation · Expired     [ Renew ]   │
│     Tidebringer                                      8 days ago      │
│                                                                      │
│ Posts you messaged                                                   │
│ ▌ The Farlands · Valheim community                 43 minutes ago  ● │
│   Eirik: Welcome! Yes, join the Discord and grab the Viking role…    │
│ ▌ ShadowFox · Valorant player                        14 hours ago    │
│   You: Want to duo tonight? Diamond support, on from 21:00.          │
└──────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** a row about the player's own post is titled with the other
  player, and its last message carries only "You:"; a row about someone else's
  post is titled with the post and names whoever wrote last.
- **Proposed:** on a desktop the open conversation sits beside the list; below
  that it has the screen to itself, with ← back to the list.

```
┌──────────────────────────────────────────────────────────────────────┐
│ Ashen                                                            ⋯   │
│ Diamond · Supporter, Lurker · Germany · EN, DE · 🎤                  │
│ About your post Night Owls · Valorant group                          │
│ ──────────────────────────────────────────────────────────────────── │
│                                Today                                 │
│ New ──────────────────────────────────────────────────────────────── │
│ ┌──────────────────────────────────────────────┐                     │
│ │ Hi, Diamond support here, mostly Killjoy and │                     │
│ │ Cypher. Free most nights after 21:00.        │                     │
│ └──────────────────────────────────────────────┘                     │
│ Ashen · 00:48                                                        │
│ ──────────────────────────────────────────────────────────────────── │
│ [ Write a message…                                        ] [ Send ] │
└──────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** a conversation about the player's own post is headed with the
  other player: their card facts when they have a post in the same game, their
  nickname otherwise, and the post it is about below. A conversation about
  someone else's post is headed with that post and its facts, with the owner's
  contacts folded under "Kestrel's contacts", or "Ways to join" for a community.
- **Proposed:** a line marks where the unread messages begin, and a run of
  messages from one side shares one line of who and when.
- **Proposed:** a conversation about an expired post says "This is an older post.
  Quill may no longer be looking."
- **Proposed:** blocking and reporting are in the ⋯ menu of the panel and of the
  conversation. Blocking asks first and says what it does; a toast then says
  "Danya is blocked." with **Undo**. A report picks one reason (spam or
  advertising; harassment, hate or threats; selling accounts, boosting or
  cheats; something else), may say more, and may also block.

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

```
┌─────────────────────────────────────────────────────────────────────────┐
│ ← Back to Valorant posts                                                │
│                                                                         │
│ ShadowFox                            Valorant player  Active 2 days ago │
│ Diamond 2 · Duelist, Initiator · Croatia · EN, DE · 🎤 · Ranked climb   │
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together. I play most evenings after  │
│  work and I'm free all weekend."                                        │
│ Age 24 · Usually online 19:00–23:00 · Agents: Jett, Raze, Sova          │
│ Playstyle: Aggressive · Tracker: tracker.gg/shadowfox ↗                 │
│                                                      [ Add on Discord ] │
├─────────────────────────────────────────────────────────────────────────┤
│ [cover]  More Valorant posts                      [ See what fits you ] │
│          Posts that fit you come first.                                 │
│          🧍 Diamond 2 · Duelist · Croatia · EN                          │
│          38 active posts                                                │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** the card is the page: its name is the page's heading and links
  nowhere, it is expanded with no **Details** left to press, and its type names
  the game, "Valorant player". A page is opened from a search result, a shared
  link or a match email as often as from the feed, where the game is overhead.
- **Proposed:** the card carries no match marks. A description is personal and
  isn't shared (7.1), and what fits is the feed's business; the way into the
  feed below the post is where the viewer's own description comes in.
- **Proposed:** the contact button is the page's one filled button. A card's
  button is outlined so a feed of twenty cards doesn't show twenty filled ones
  (14.1), but a page holds one card, and contacting it is what the page is for.
- **Proposed:** the way into the feed carries the game's cover, how many posts
  are active and what the feed will show: what fits the viewer's description,
  with that description under it, or the whole feed while they have none.
- **Proposed:** on the owner's own post the page is their view of it, as on the
  home page (11.2): **Edit** and **Renew** in place of the contact button, the
  state, the conversations and the contact reveals under the facts, and **See
  what fits**, which opens the feed with the description taken from this post,
  in place of the invitation to browse.
- **Proposed:** **Back to Valorant posts** shows only while the feed is the
  page behind, and is the browser's own Back, which is what keeps the batches. A
  page opened from a link has no feed behind it, and the way into the feed below
  the post is its only one.
- **Proposed:** a visitor to an expired post is told it is old before they
  write: "This is an older post. ShadowFox may no longer be looking, but you can
  still write." Its owner reads the post's state instead, which says what
  expiry means for them.
- **Proposed:** a blocked player's post keeps its page without its contact
  button (section 10), under a line saying why, so nothing reads as broken. The
  line links to the blocked list on the account page (11.5), which is where the
  block is undone.

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
- **Proposed:** signed out, a line under "What are you posting?" says what the
  site is: "Find players, groups and communities for the games you play. Post
  once, and we'll tell you when someone new fits." The cover grid follows under
  "Or browse a game".

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Your posts                                                              │
│                                                                         │
│ ┌───────┐  Kestrel  PLAYER                                              │
│ │ cover │  Croatia · HR, EN · Mic · Casual, Building                    │
│ │       │  ! Expires in 3 days · No conversations yet ·                 │
│ │       │  Contacts shown 2 times                                       │
│ └───────┘  [ See what fits ]  Edit  [ Renew ]                           │
│            + New Valheim post                                           │
│                                                                         │
│ ┌───────┐  Night Owls  GROUP  3 players, wants 2 more                   │
│ │ cover │  Platinum – Diamond · Lurker, Supporter · EU · EN, HR         │
│ │       │  Active for 24 more days · 3 conversations ● 1 unread ·       │
│ │       │  Contacts shown 14 times                                      │
│ └───────┘  [ See what fits ]  Edit  Renew                               │
│            + New Valorant post                                          │
│ ─────────────────────────────────────────────────────────────────────── │
│ Other games                                                             │
│ [cover] [cover] [cover] [cover]                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** a post on the home page is its card's heading, which opens the
  post's page (11.1), and its fact line, then its state, its conversations and
  how often its contacts were shown, then **See what fits**, **Edit** and
  **Renew**. The owner's words stay off it: the owner wrote them.
- **Proposed:** the owner is the one player who knows a post expires, so the
  state counts forward: "Active for 24 more days"; in its last week, when the
  renewal email goes out, "Expires in 3 days" in full weight with an icon; after
  that, "Expired 3 weeks ago", saying that it is listed under older posts and
  that match emails are paused.
- **Proposed:** **Renew** is plain text while the post is active and outlined in
  its last week or once expired. The page has no filled button: no one of its
  actions is the thing to do.
- **Proposed:** the conversation count opens the inbox on the post's first
  conversation with unread messages, in the inbox's order, or on its first
  conversation when none are unread. Contact reveals read "Contacts shown 14
  times", and are left out while there are none.
- **Proposed:** a game's cover stands beside its posts with no title, their top
  edges level: the logo on the cover names the game, and the cover opens its
  feed. On a phone the cover is too small to read, so the name sits beside it,
  above the posts. While the player lacks a post type in the game, "New
  Valorant post" below the last post starts the flow at the type step with the
  game known. Games keep the catalogue's order, so renewing doesn't move a post.

### 11.3 Notification list

- **Decided:** notifications are a dropdown list in the header, and each opens
  what it is about:

| Notification                          | Opens                         |
| ------------------------------------- | ----------------------------- |
| A new post fits one of yours (8)      | That post's page (11.1)       |
| One of your posts is about to expire  | The home page (11.2)          |

- **Decided:** the list is grouped by the player's own posts, each notification
  under the post it is about, so a player with two posts in a game can tell them
  apart. The heading names the post as its card does, "Night Owls · Valorant
  group", and is plain text: the rows are what open anything.
- **Proposed:** the post with the newest notification comes first, so what just
  happened leads. Inside a post its own expiry comes first, since it is the row
  with something to do, and the posts that fit follow, newest first.
- **Decided:** every fitting post is its own notification, never merged with
  others (section 8).
- **Proposed:** a row about a post that fits names it and its type, and carries
  the time it was published, the same relative time a card's freshness uses:
  "NightHell fits · Player · 4 weeks ago".
- **Proposed:** the expiry notification reads the post's state now, in the words
  the home page gives its owner, rather than recording the moment it fired:
  "Expires in 3 days", then "Expired 3 weeks ago". It carries no time of its
  own, since its text already says when, and renewing the post takes it away.
  There is one per post, never a second.
- **Proposed:** unread rows carry a dot, and **Mark all read** in the heading
  row clears them all; it shows only while something is unread. The count on the
  bell is how many are unread, and opening a notification reads it.
- **Proposed:** a notification is about two posts, the player's own and the one
  that fits it, and deleting either takes it away, as deleting a post takes its
  conversations (section 10). Nothing else removes one: a row whose fitting post
  has since expired still stands and opens that post's page, which says so
  (11.1).
- **Decided:** messages aren't notifications. The inbox's link in the header shows
  how many conversations are unread.
- **Proposed:** contact reveals are counts on the home page, never notifications.
- **Proposed:** the list scrolls inside the dropdown rather than paging, and
  there is no page of notifications to send it to. The oldest fall off once a
  player has more than the list holds.
- **Proposed:** with nothing in it the list says so and offers the way out of
  that: "No notifications yet. Every post tells you when someone new fits it,
  and before it expires", with **New post**. A player without posts can't be
  told anything.
- **Decided:** on a phone the list opens full-screen, like Games (11.4), and
  **Mark all read** sits above it, where the heading row is the screen's own.

```
┌──────────────────────────────────────────┐
│ Notifications              Mark all read │
│                                          │
│ Night Owls · Valorant group              │
│ ● Vex fits                               │
│   Player · just now                      │
│ ● NightHell fits                         │
│   Player · 4 weeks ago                   │
│   Tatami fits                            │
│   Player · 7 weeks ago                   │
│                                          │
│ Kestrel · Valheim player                 │
│ ● Expires in 3 days                      │
│   Renew it from your posts.              │
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
│ ◆  Games ▾          [ + New post ]  ☰  │ │ ◆  Games ▾         [ + ]  ✉ 2  🔔 3  K │
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

- **Decided:** the logo leads home: the start of post creation signed out, the
  player's posts signed in (11.2).
- **Decided:** **Games** opens the cover grid, and each cover opens that game's
  feed. For a signed-in player, games where they have a post carry a mark.
- **Proposed:** the grid holds the whole catalogue at once, in about two rows, on
  covers smaller than the home page's and larger than a phone's, so nothing is
  scrolled or searched for (14.3). The mark reads "Your post", or "Your posts"
  where there are several: the header knows no post type, so it marks the game
  rather than a post of the type being written, as posting's grid does.
- **Decided:** **New post** is the header's one prominent button, signed in or
  out, since it is the start of the funnel. It is outlined, not filled, so it
  doesn't compete with the one filled button of the page below it (14.1), such as
  **Publish post** on the feed. **Sign up** is a plain link: most
  players register through New post, and the rest are asked to when they first
  open a contact panel.
- **Decided:** the inbox icon opens the inbox page and shows how many
  conversations are unread. The bell opens the notification list (11.3).
- **Proposed:** the icon of the page the viewer is already on takes full weight,
  where the others are muted.
- **Decided:** the account menu holds **Your posts**, **Account** and **Sign
  out**, under the player's nickname. Your posts repeats the logo's destination,
  since nobody expects the logo to lead to their own posts.
- **Proposed:** **Sign out** lands on the home page: the page the player was on
  may have been theirs, and signed out the home page is what the site is for.
- **Decided:** on a phone the logo shrinks to its mark, and New post stays
  visible: with its label signed out, and as the plus alone signed in, where the
  two counts and the account button share the row with it. Signed out, a menu
  holds Sign in and Sign up. Games and notifications open full-screen, like the
  description bar, and the account menu opens as a sheet from the bottom, which
  is as tall as the few rows it holds.
- **Decided:** one menu is open at a time. A click outside closes it, and so
  does Escape, which gives the focus back to the button that opened it; opening
  one puts the focus on its first item, and a phone's full-screen menu keeps it.

### 11.5 Account page

```
┌───────────────────────────────────────────────────────────┐
│ Account                                                   │
│                                                           │
│ Shown on your posts                                       │
│ Every post you have shows these, as your account has them │
│ now. Change one here and it changes on all 3 at once.     │
│   Nickname    Kestrel                                     │
│   Birthday    12 April 1998, shown as age 28              │
│   Location    Croatia                                     │
│   Languages   Croatian, English                           │
│   Timezone    Europe/Zagreb                               │
│   Contacts    Discord kestrel · Riot ID Kestrel#EUW       │
│ [ Edit ]                                                  │
│                                                           │
│ Only you see this                                         │
│   Email       kestrel@example.com                  Change │
│   Sign-in     Email and password                   Change │
│   Emails      [✓] Matches  [✓] Messages  [✓] Renewals     │
│   Blocked     Danya                               Unblock │
│                                        [ Delete account ] │
└───────────────────────────────────────────────────────────┘
```

- **Decided:** one account page with two sections. **Shown on your posts** holds
  the facts about the player and their contacts, which posts show live (section 6,
  step 3), so editing them changes every post at once. **Only you see this**
  holds the email, how the player signs in, email switches, blocked players and
  deleting the account.
- **Decided:** the email and the way the player signs in are rows of their own.
  Discord knows an account by its Discord id, so the email is where the site
  writes whether the player signs in with it or with Discord, and a Discord
  player changes it without leaving Discord.
- **Decided:** sign-in shows the player's one way of signing in, an email and
  password or Discord, and changes it either way. Moving to Discord takes the
  password's place and leaves the email as it is; moving to a password signs in
  with the address the account already holds, and asks for one where it has
  none. Discord stays on the posts as a contact either way. Either move is
  refused where what it would sign in with already signs in to another account:
  the Discord, or the address the account holds, which the player then changes.
- **Proposed:** the email row says where the address stands (section 6, step 4):
  confirmed, waiting for its link with **Send again**, or missing, which says
  what the site can't tell the player about. Changing it asks the new address to
  confirm itself.
- **Proposed:** match, message and renewal emails each have an on/off switch,
  each saying what it would send. Every email carries an unsubscribe link, and it
  lands on these switches. While the address is unconfirmed or missing, the
  switches say that nothing is sent whatever they are set to.
- **Decided:** blocked players are listed with **Unblock**, which brings back
  what the block hid (section 10). The list stands in the section rather than
  behind a button of its own: it holds a few names at most, and a blocked
  player's post links to it (11.1).
- **Decided:** **Delete account** deletes the account's posts and conversations
  (section 10), and says so, with the counts, before it does.
- **Proposed:** the facts and contacts are edited together, under one **Edit**,
  rather than a row at a time: they are given together when a post is written,
  and the line under the heading says what changing one reaches. **Edit** sits
  under the list, where **Save changes** takes its place.
- **Proposed:** a game account (Riot ID, Steam profile, EA ID) belongs to the
  games played with it, so the page asks for every kind and says where each
  shows. A post asks only for the one its game uses (section 6, step 3).
- **Proposed:** the page has no filled button until something is being edited,
  where **Save changes** is it. **Delete account** is outlined in error (14.1).

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
| Player location                    | The country it names, else nothing           |
| Platforms (Steam, Origin, ...)     | Platform, every PC store as PC               |
| Alert                              | Nothing, or a post for its owner (open)      |

- **Proposed:** a location carries over only where it names a country. Three in
  four do not: they name a region of today's tree, most often just Europe or
  North America, and those players arrive without one until the account page asks.
- **Decided:** existing alerts come from anonymous emails and have no place in the
  new model. None carry over. Everything else is imported as far as it converts,
  and the rows that don't fit are dropped.
- **Open:** a one-time relaunch email to existing players and alert subscribers.
- **Open:** Search Console shows few paginated feed pages were ever indexed. Watch
  indexing of posts and feeds after launch.
- **Decided:** the relaunch is judged by feed views, descriptions started, posts
  published, post views, contact reveals, conversations started, renewals and
  returning visits. Measuring them needs a product that follows one visitor
  through the funnel and counts the ones who come back, and picking one is out of
  scope for the first release: it is a consent question as much as a tooling one
  for an audience that is largely European. The relaunch ships without analytics,
  and the eight are what the product is chosen against when it is.
- **Decided:** contact reveals are counted on the post itself whatever else is
  measured, since the home page shows an owner their own (11.2).

## 13. Data model gaps

The model is redesigned from this brief, not before it. What the brief needs that
the current model lacks:

- **Posts.** Player, group and community posts in place of player profiles, teams,
  team profiles and alerts, with at most one post per player, type and game.
- **Group fields.** How many the group is and how many more it wants, needed
  roles, a rank range, an age range, a set of regions, and an optional name,
  website and Discord server.
- **Community fields.** Name, an age range, Discord invite and website.
- **One free text field** in place of about and ambitions, and a **Looking for**
  value on all three post types.
- **Game fields.** Which fields appear on a card, whether a field takes one option
  or several, and platform and the Looking for options as per-game fields with
  per-game options.
- **Regions.** Group and community regions, and the mapping from every country to
  one of the twelve (5), in place of today's three-level tree, whose nodes a
  player can be in at any depth. Matching reads the mapping in the database; the
  post screen and the account page read the same lists from an endpoint, rather
  than carrying 229 countries in the bundle.
- **Freshness.** A last renewal or edit time that sorts the feed and decides expiry,
  30 days for player and group posts and 90 for communities.
- **Renewal links** that renew one post without signing in.
- **One feed query** across all three post types, sorted and batched together.
- **Matching.** How many fields of a post match a description or another post
  (7.2), per pair of post types, including rank closeness from the game's ordered
  rank options; usable as a sort key and as a cursor for Load more, and degrading
  to plain recency when the description is empty. Counted in the query, not after
  it: Load more continues from a match count and a renewal time together (4), and
  a score the database didn't produce can't be a cursor.
- **Match notifications.** One per fitting post, fired on publish and on renewal
  after expiry, for the owners of posts it fits, with emails that carry several.
- **Messaging.** Conversations tied to a post, messages, unread state, blocks and
  reports.
- **Notifications.** An on-site notification list and badge.
- **Contact preference** on posts, and a count of contact reveals per post.
- **Email switches** on the account, one per kind of email.
- **A confirmed address.** Whether the account's email is confirmed, and the link
  that confirms it. Today's schema holds neither, and its check that an account
  has a password or a Discord id, never both, is what the sign-in row changes.

## 14. Design system

The design system is extracted from the prototypes. This section is what they
start from: the palette, type, icons and layout they share, and the components the
brief's screens already call for.

### 14.1 Color

- **Decided:** dark by default. Game covers are the site's only per-game art and
  carry most of its color. They stand out on dark, and players come from Discord,
  Steam and trackers, which are dark too.
- **Decided:** the palette comes from the tavern: gray stone, brown wood, green
  nature and red fire. Cards are tables on a stone floor.
- **Proposed:** tokens are CSS custom properties named by role, never by value, so
  a light theme is a second set of values for the same names. No light theme is
  designed for the first release.
- **Proposed:** the values, each checked against WCAG AA on every surface it is
  used on: 4.5:1 for text, 3:1 for control edges.

| Token        | Value     | From   | Used for                                          | Lowest contrast |
| ------------ | --------- | ------ | ------------------------------------------------- | --------------- |
| floor        | `#141210` | stone  | Page background                                   |                 |
| table        | `#1F1A16` | wood   | Cards                                             |                 |
| raised       | `#2A241F` | stone  | Panels, menus, inputs, the description bar        |                 |
| border       | `#3B332C` | stone  | Card edges, dividers                              | decorative      |
| input-border | `#7F7366` | stone  | Edges of inputs and controls                      | 3.3             |
| text         | `#EFE9E2` |        | Headings and body text                            | 12.7            |
| text-muted   | `#AFA397` |        | Freshness, labels, ≠ marks                        | 6.2             |
| text-faint   | `#968A7D` |        | Expired cards, placeholders                       | 4.5             |
| ember        | `#F2823F` | fire   | Primary buttons, links, focus ring, unread counts | 5.9             |
| ember-hover  | `#F59A62` | fire   | Hover on ember                                    |                 |
| on-ember     | `#1A0F0A` |        | Text on ember buttons                             | 7.2             |
| moss         | `#8FC06E` | nature | ✓ marks                                           | 7.3             |
| error        | `#F0587A` |        | Form errors, destructive actions                  | 4.7             |

- **Proposed:** fire is scarce and marks what asks for a click: one filled ember
  button per screen, links, the focus ring and unread counts. A card's contact
  button is outlined, so a feed of twenty cards doesn't show twenty filled buttons,
  and so is the header's **New post** (11.4).
- **Proposed:** green means one thing, a field that fits. ≠ is muted stone, never
  error: a mismatch is information (7.2), and a thin feed full of red would look
  broken. Both marks carry their glyph, so neither relies on color. For the same
  reason "Your post is live" carries an ember party popper, not a green check.
- **Proposed:** error is a rose kept apart from the ember, and always comes with
  an icon and a message.
- **Proposed:** an expired card keeps the table surface and its text drops to
  text-faint. It fades without turning transparent, so it still passes contrast.
- **Proposed:** post types have no color of their own, only a label and a glyph
  (14.3). Type colors would compete with the match marks and the covers.

### 14.2 Type

- **Decided:** Inter, self-hosted with the Latin, Latin Extended, Cyrillic and
  Greek subsets, since posts are written in many languages.
- **Proposed:** tabular figures for hours, ages, counts and slots, so they line up
  down the feed.
- **Proposed:** five sizes, 12, 14, 16, 20 and 28 px, in weights 400, 500 and 600.
  A card uses four steps: the heading (name, type, freshness) at 16/600, the fact
  line at 14/500, labels at 12/500, and the player's words at 14/400. The fact line is
  the most read text on the site, so it gets the strongest body weight.

### 14.3 Icons

- **Decided:** Lucide, inlined as SVG. No icon font.
- **Decided:** no per-game icons. Ranks, roles and agents are text, and the cover
  stays a game's only asset, so a new game is still one seed file and one cover.
- **Decided:** the cover grid shows no titles. Each cover carries its game's logo,
  readable at the grid's tile size and naming the game as the site does, and the
  title is the tile's accessible name. A cover without such a logo doesn't
  qualify.
- **Decided:** no search above the cover grid while the catalogue fits in about
  two rows on a desktop. Recognising a logo is faster than typing a name.
- **Proposed:** the set the brief needs: user, users and castle for the three post
  types; mic, bell, mail, copy, check, not-equal, chevron, close, plus, menu,
  external link and alert; megaphone for the prompt to publish and party popper
  for a post gone live; and Discord's own mark on Discord buttons, which Lucide
  doesn't carry.

### 14.4 Layout and motion

- **Proposed:** spacing on a 4 px grid.
- **Proposed:** the feed is one column about 720 px wide, not a grid: cards are
  compared, and comparing reads down. On a desktop that leaves room on either side
  for the ad rails section 15 expects.
- **Proposed:** two breakpoints. Below 640 px is a phone, where overlays go
  full-screen (4, 5.6, 7.1, 11.4); from 1024 px is a desktop, where the rails
  appear.
- **Proposed:** a card's expansion animates its height, and doesn't animate under
  reduced motion.

### 14.5 Components

What the brief's screens already call for; the prototypes settle their shape.

| Area    | Components                                                                   |
| ------- | ---------------------------------------------------------------------------- |
| Card    | The card shell, fact line, match mark, slot pips, freshness label, the already-messaged mark, the home page's own post with its state and counts |
| Feed    | Type chooser (radio cards, also step 1 of posting), description bar with **More**, segmented control, publish prompt and its muted line for the viewer's own post, tier heading, labelled divider, **Load more**, a post page's way into the feed |
| Inputs  | Pills for a field's few options, one or several; tokens for a few out of many, such as languages; select, range picker (rank, age), an hours range that crosses midnight, count stepper ("3 players, want 2 more"), checkbox, radio group, text area with prompts, an account fact with **Change** |
| Overlay | Modal, side panel, full-screen sheet, sheet from the bottom, dropdown         |
| Contact | Contact panel, contact row with **Copy**, message box, conversation thread, the unread line, the ⋯ menu with Report and Block |
| Lists   | Notification rows grouped by post, inbox rows, unread badge and dot, the account page's definition list, toggle switch, a confirmation that shows counts, a toast with **Undo** |

- **Proposed:** one card shell for all three types, with fixed rows: heading,
  facts, the player's words and the footer. Types differ in what fills the rows
  and in how many lines of text show before the cut: two for players and groups,
  four for communities. This is the first answer the prototypes try to the mixed
  card sizes in section 4.
- **Decided:** on a phone, a group or community card's footer puts "Posted by" on
  a row of its own, and the contact button and **Details** share the row below
  it, so **Details** never wraps onto a line by itself.
- **Proposed:** a match mark has two states, fits and doesn't, and sits on the
  fact it is about (5.6). A field that isn't compared carries no mark.
- **Proposed:** one overlay component, shown as a modal or side panel on a
  desktop, a full-screen sheet on a phone, or a dropdown from the header. The
  description bar, the contact panel, Games, notifications and the account menu
  all use it. It traps focus while open and returns it on close. A menu of a few
  short rows opens as a sheet from the bottom of a phone rather than
  full-screen, which would read as a page (11.4).
- **Proposed:** buttons come in three weights: filled ember, once per screen;
  outlined, for card contact buttons, the header's **New post** and secondary
  actions; and plain text, for
  **Details ⌄**, **Sign in** and the like. Destructive actions are outlined in
  error.

### 14.6 Voice

- **Proposed:** the brief's own copy sets the voice: "Fits you", "Missing one
  thing", "Older posts · they may no longer be looking".
- **Proposed:** sentence case everywhere. Choices are in the player's voice ("I'm a
  player looking for a group", "We're a group looking for players"). The site
  never says "team". A thin or empty state ends with something to do: "Publish
  your post and we'll tell you when someone fits."

### 14.7 Logo

- **Decided:** the logo is a hearth flame in ember, the TeamTavern wordmark
  beside it in the header. `redesign/logo/mark.svg` is the flame alone,
  transparent and unpadded, for the header and anywhere else the site shows its
  mark; on a phone it stands alone at 24 px (11.4). The mark is the site's
  identity only, never a UI icon, so no Lucide flame stands beside it either.
- **Decided:** `redesign/logo/favicon.svg` is the same flame, padded on a
  rounded tile of the floor colour, so it reads on a browser tab whatever the
  tab's colour. It is the site's favicon.

### 14.8 Prototype content

- **Proposed:** prototypes use content from the production dump rather than
  placeholder text, and include:
  - about texts in several languages and scripts;
  - long nicknames and group names;
  - empty fields, especially online hours (56% filled);
  - Valheim, which has no rank, so a group's heading reads "Wants 2–3 more";
  - a game whose posts have all expired, so the divider is at the top;
  - an expanded card with many game fields;
  - every overlay at phone width.

## 15. Not covered yet

- Competitions and leagues.
- Ads. Every layout in this brief is ad-free. Ads fit around the design rather
  than the design around them, so they are placed last. Side panels and a bottom
  sticky are likely; the rest is open.

## 16. Out of scope

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
