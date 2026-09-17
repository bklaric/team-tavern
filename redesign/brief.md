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
- **Renewal**: one click in an email, or an edit, that makes a post active again.

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
- 97% of players have exactly one profile.

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
| **Player**    | One person                                    | A group, a community, or someone to duo with | Message |
| **Group**     | A few people who need more players            | Players              | Message          |
| **Community** | A clan, server or Discord open to many        | Players              | Join, or message |

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
│         38 posts active this week                                       │
├─────────────────────────────────────────────────────────────────────────┤
│ (•) I'm a player looking for a group                                    │
│ ( ) We're a group looking for players                                   │
│ ( ) We're a community looking for members                               │
│                                                                         │
│ Diamond 2 ▾  Controller, Sentinel ▾  EU West ▾  EN, DE ▾  19–23 ▾  🎤   │
│ More ▾                                                                  │
├─────────────────────────────────────────────────────────────────────────┤
│ ✦ Publish this as your post: groups and players can find you too, and   │
│   we'll tell you when someone new fits.               [ Publish post ]  │
├─────────────────────────────────────────────────────────────────────────┤
│ Showing  [ All ]  [ Groups ]  [ Communities ]  [ Players ]              │
│  group card      ✓ Needs Controller · ✓ Diamond fits · ✓ EU · ✓ 21–23   │
│  community card  ✓ EU · ✓ EN · ✓ PC                                     │
│  player card     ✓ Diamond 1 · ✓ EU · ✓ 19–22                           │
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
- **Decided:** active posts come first, then a divider, then expired posts in the
  same list. Nothing is hidden. When a game has no active posts, the divider is at
  the top.
- **Decided:** within each of those, posts are grouped by how many fields match the
  viewer's description and ordered by last renewal or edit inside each group (7.2).
  An empty description matches nothing in particular, so the feed is simply ordered
  by last renewal or edit.
- **Decided:** a **Load more** button replaces pagination. Nothing loads
  automatically. Each batch continues after the last shown post's sort position
  (match count and renewal time), not by offset, so renewals during browsing don't
  duplicate or skip posts.
- **Decided:** renewal is allowed at any time and moves the post up. Bumping has
  never been a problem, so nothing prevents it.
- **Proposed:** the header shows how many posts are active, not the total, which
  overstates activity.
- **Proposed:** pages whose posts are all expired are kept out of search engines.
- **Proposed:** cards expand in place (5.4), so browsing the feed doesn't navigate
  away and Back doesn't discard the loaded batches. Opening a post's own page from
  its heading still does; whether that page returns to the feed intact is open.
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
- Contacts are revealed on click, never shown up front.

**Proposed:** which game fields count as decisive is set per game, for example
rank and role for Valorant, rank and position for LoL. Either the first fields by
order or a "shown on card" flag per field.

### 5.1 Player card

```
┌─────────────────────────────────────────────────────────────────────────┐
│ ShadowFox                                  PLAYER    Active 2 days ago  │
│ Diamond 2 · Duelist, Initiator · EU West · EN, DE · 19–23 CET · 🎤      │
│ [ Ranked climb ]  [ Returning player ]                                  │
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together…"                            │
│                    [ Show contacts ▾ ]  [ Message ]        Details ⌄    │
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
│ Posted by Kestrel  [ Show contacts ▾ ]  [ Message ]        Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** the open slots and needed roles are the headline, where a player
  card has rank. They are what a solo player scans for.
- **Proposed:** ranges and sets replace single values: rank range, age range, one
  or more regions.
- **Proposed:** a name is optional; without one the heading reads "Kestrel's group".
- **Proposed:** "Posted by" names the owner, since conversations are between players.
- **Proposed:** for server games the headline reads "Wants 2–3 more on our server",
  and rank disappears where the game has none.
- **Proposed:** "Organized" is a tag, with the website shown in details.

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
│ Run by Eirik · farlands.gg                                              │
│                    [ Message ]  [ Join Discord ↗ ]         Details ⌄    │
└─────────────────────────────────────────────────────────────────────────┘
```

- **Proposed:** the main action is **Join Discord**, **Visit site** or
  **Message**, depending on how the community says people join (section 6, step 5).
  The others stay available as secondary actions.
- **Proposed:** the pitch gets more room than on other cards, because for a
  community the text is the product.
- **Proposed:** the Discord invite and website are public links, not hidden
  contacts. Opening them requires sign-in, like other contacts.
- **Proposed:** a community carries a focus (Casual, Competitive, Events, Roleplay,
  Learning) and an experience level, shown as tags.
- **Open:** community logos. They need uploads and moderation.

### 5.4 Expanding a card

```
│ "Peak Immortal last act, looking for a consistent duo. Chill but I want │
│  to improve, happy to review VODs together. I play most evenings after  │
│  work and I'm free all weekend."                                        │
│                                                                         │
│ Weekdays 19–23 CET · Weekends 14–02 CET · Age 24                        │
│ Agents: Jett, Raze, Sova · Playstyle: Aggressive · Ranked acts: 6        │
│ Tracker: tracker.gg/shadowfox ↗                                         │
│                    [ Show contacts ▾ ]  [ Message ]        Details ⌃    │
```

- **Proposed:** a card has one expansion, not two. Expanding shows the full text
  and the remaining fields together; there is no separate **Read more**.
- **Decided:** only the **Details ⌄** button expands the card, not the card itself.
  The card grows in place; the feed doesn't navigate.
- **Proposed:** contacts stay a separate reveal, since they are gated and counted
  (5.6).
- **Proposed:** the nickname, group name or community name links to the post's own
  page, which is what is shared, crawled and opened in a new tab.
- **Proposed:** what sits behind the expansion, by type:

| Type      | On the card                                                        | Behind Details                                                            |
| --------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| Player    | Rank, roles, region, language, one schedule line, microphone, Looking for | Weekday and weekend hours, age, remaining game fields, trackers      |
| Group     | Open slots, needed roles, rank range, regions, language, schedule, microphone, ages | Weekday and weekend hours, remaining game fields, website, Discord server |
| Community | Kind, region, language, platform, focus, experience level          | Remaining game fields, the rest of the pitch, website                     |

### 5.5 Free text and "Looking for"

- **Proposed:** about and ambitions merge into one field, "About you and what you're
  looking for". Ambitions are left empty most often, overlap with about when both
  are written, and are nearly always one of a few answers.
- **Proposed:** a structured **Looking for** choice on player and group posts:
  Casual, Ranked climb, Competitive or tournaments, Friends or community. It is a
  card tag and a matching field.
- **Proposed:** "New or returning player" stays, as a tag.

### 5.6 Card actions

- **Proposed:** in the feed, each card lists what fits the viewer's description
  ("✓ Needs Controller", "✓ Diamond fits") and what doesn't ("≠ Platinum 1–2"),
  which is what makes the fuzzy ordering (7.2) legible.
- **Decided:** a post offers both on-site messaging and off-site contacts
  (Discord tag, Riot ID and the other game accounts).
- **Decided:** messaging and revealing contacts both require sign-in.
- **Proposed:** the owner states a preference in the contact step (section 6,
  step 5), and the card's main action follows it.
- **Proposed:** revealing contacts is counted, so owners can be told how often it
  happens.

## 6. Post creation

**Decided:** onboarding and post creation are one flow. Signed out, it ends with
registration; signed in, it skips it.

**Decided:** on the feed, the viewer's description already covers the type, game
and essentials. **Publish post** continues the flow from **In your words**. The
full flow below is for entry points that start from nothing.

```
Type ─▶ Game ─▶ Essentials ─▶ In your words ─▶ Contact ─▶ Preview ─▶ Register ─▶ Matches
         (skipped when known)                                     (signed out only)
```

### Entry points

| From                                   | Skips                    |
| -------------------------------------- | ------------------------ |
| "Publish post" on the feed             | Type, game, essentials   |
| Header or home page                    | Nothing                  |
| Right after registering                | Registration             |

### Steps

1. **Type.** The same choice as on the feed, in the same words, each with a short
   example.

```
┌───────────────────────────────────────────────────────────┐
│  What are you posting?                                    │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ 🧍  I'm a player looking for a group                 │  │
│  │     Post yourself so groups and players find you    │  │
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

   - **Decided:** if the player already has a post of that type for the game, the
     flow shows it and offers **Edit it** or **Delete it**. Editing opens the flow
     prefilled.
   - **Open:** "Just browsing" as a fourth choice that skips posting.
2. **Game.** The cover grid, as in the header and home page.
3. **Essentials.** Only the fields the card shows, beside a live preview of the
   card that fills in as the player types.

```
┌──────────────────────────────────────┬──────────────────────────────────┐
│ Valorant · group                     │ Preview                          │
│                                      │ ┌──────────────────────────────┐ │
│ How many are you, and how many do    │ │ Night Owls            GROUP  │ │
│ you want in total?                   │ │ ●●●○○ 3 of 5 · Needs         │ │
│   [ 3 ] of [ 5 ]                     │ │ Controller, Sentinel         │ │
│ Roles you need   [Controller][Sent…] │ │ Plat 1 – Dia 3 · EU · EN     │ │
│ Rank range       [Plat 1] – [Dia 3]  │ │ 21–01 CET · 🎤               │ │
│ Region           [EU ▾]              │ │ [ Ranked climb ]             │ │
│ Language         [EN ▾]              │ └──────────────────────────────┘ │
│ Usually online   [21:00] – [01:00]   │                                  │
│ Looking for      (•) Ranked climb    │                                  │
│ Microphone       [✓] Required        │                                  │
│ Group name       [Night Owls] optional                                  │
│                                      │                                  │
│ ⌄ Add more details (optional)        │                                  │
│   Weekday and weekend hours, agents, playstyle, website, server         │
│                              [ Next ]│                                  │
└──────────────────────────────────────┴──────────────────────────────────┘
```

   - *Player:* rank, roles, region, language, schedule, microphone, looking for.
   - *Group:* size and open slots, needed roles, rank range, regions, language,
     schedule, microphone, age range, looking for, optional name.
   - *Community:* name, focus, experience level, region, language, platform,
     Discord invite, website.
   - **Decided:** the player details step (birthday, location, languages,
     timezone) stays and stays skippable; players fill it in willingly.
   - **Proposed:** online times move next to the preview, where their effect on
     the card is visible.
   - **Proposed:** facts about the player are asked once, kept on the account and
     prefilled into later posts.
   - **Proposed:** fields that don't appear on the card sit in a collapsed
     **Add more details (optional)** section of this same step, not a step of
     their own. The live preview shows why they are separate: filling them in
     doesn't change the card.
   - **Proposed:** the Matches screen nudges once more for players who skipped it:
     "Add more details to your post".
4. **In your words.** One text field with prompts and an example.

```
┌───────────────────────────────────────────────────────────┐
│ Tell people about your group                              │
│ ┌───────────────────────────────────────────────────────┐ │
│ │                                                       │ │
│ └───────────────────────────────────────────────────────┘ │
│ Ideas: How do you play? What kind of people get along     │
│ with you? What are you aiming for this season?            │
│                                                           │
│ Example: "Three friends who play most nights, we want to  │
│ stop solo queuing for the last two spots. No tilt…"       │
│                                        [ Skip ]  [ Next ] │
└───────────────────────────────────────────────────────────┘
```

   - **Proposed:** optional for player and group posts, required for communities.
5. **Contact.** How people reach the post, prefilled from the account. On-site
   messaging always exists, so no post is unreachable and every off-site contact
   is optional. The answer decides the card's main action.

```
Player and group                            Community
┌─────────────────────────────────────────┐ ┌─────────────────────────────────────┐
│ How should people reach you?            │ │ How do people join?                 │
│ (•) Message me on TeamTavern            │ │ (•) Discord invite [discord.gg/…  ] │
│ ( ) Add me on Discord or in game        │ │ ( ) Website        [https://…     ] │
│ ( ) Either is fine                      │ │ ( ) They message me first           │
│                                         │ │                            [ Next ] │
│ Contacts shown to signed-in players:    │ └─────────────────────────────────────┘
│ Discord [        ] [ Sign up with Discord ]
│ Riot ID [        ]                      │
│                                [ Next ] │
└─────────────────────────────────────────┘
```

   - **Proposed:** player and group posts ask "How should people reach you?":
     message me on TeamTavern, add me on Discord or in game, or either, followed by
     the game's contact fields. A group may also give a Discord server invite.
   - **Proposed:** community posts ask "How do people join?": a Discord invite, a
     website, or "they message me first". No link is mandatory; 71% of today's
     communities have a Discord server and 30% a website, but some vet applicants
     or run on a game server.
   - **Proposed:** the main action follows: **Message**, **Show contacts**,
     **Join Discord**, or **Visit site**.
   - **Proposed:** a community's renewal email asks whether the invite link still
     works, since nothing checks it.
   - **Decided:** when signed out, a **Sign up with Discord** button sits beside the
     Discord input. It saves the draft, registers or signs in with Discord, returns
     to this step with the Discord tag filled in, and removes the register step.
6. **Preview.** The card exactly as it will appear.

```
┌───────────────────────────────────────────────────────────┐
│ This is how your post will look                           │
│ ┌───────────────────────────────────────────────────────┐ │
│ │ (full card, exactly as in the feed)                   │ │
│ └───────────────────────────────────────────────────────┘ │
│ Your post stays active for 30 days. We'll email you       │
│ before it expires so you can renew in one click.          │
│ We'll tell you when someone new fits your post.           │
│                                          [ Publish post ] │
└───────────────────────────────────────────────────────────┘
```

   - **Proposed:** the renewal rule is stated: "Your post stays active for 30 days.
     We'll email you before it expires so you can renew in one click." Communities
     read 90 days.
   - **Proposed:** the notification rule is stated: "We'll tell you when someone new
     fits your post."
7. **Register** (signed out, without Discord sign-up). The draft is kept through
   registration.
   - **Decided:** the flow always asks for a nickname. Discord registration may
     prefill it, and the player can edit it.
8. **Matches.** "Your post is live", followed by posts that fit it.

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
- **Decided:** the description is a post draft. Publishing it continues post
  creation from **In your words** (section 6), and the feed invites it: "Publish
  this as your post: groups and players can find you too, and we'll tell you when
  someone new fits."
- **Proposed:** the draft is stored locally, so seeing what fits needs no account.
  Publishing does.
- **Proposed:** a player with a post of that type for the game gets the description
  prefilled from it. Changing the description offers to update the post.
- **Proposed:** the bar carries the card's fact fields for the chosen type; the rest
  sits under **More**. On mobile the type stays visible and the fields open in a
  sheet.

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

**Decided:** posts are grouped under headers by how many fields they match, so the
ordering explains itself.

**Proposed:** what a field compares against depends on the two post types, not on
the field:

| Viewer is a | Shown       | Fields compared                                                                 |
| ----------- | ----------- | -------------------------------------------------------------------------------- |
| Player      | Groups      | The viewer's roles against the group's needed roles; their rank inside its range; region, language, platform, age, hours, microphone, Looking for |
| Player      | Players     | Rank closeness; region, language, platform, age, hours, microphone, Looking for; shared roles count as a match |
| Player      | Communities | Region, language, platform; Looking for against the community's focus            |
| Group       | Players     | The group's needed roles against the player's roles; the group's rank range against their rank; the rest as above |
| Community   | Players     | Region, language, platform; the community's focus against their Looking for      |

- **Proposed:** hours overlap in the viewer's timezone, and "near" rank is a few
  steps either way in the game's ordered rank options.
- **Decided:** every field counts the same. Ordering posts within a group by which
  fields matter more is out of scope until there is usage data to base it on.
- **Out of scope:** per-field "must match" toggles. Nothing is ever excluded, so a
  Portuguese-speaking player still sees English-only posts, marked `≠ EN`, below
  the ones that fit. With supply this thin that is worth more than a shorter, more
  correct page.
- **Proposed:** fields that don't apply between two types, such as rank against a
  community, count neither for nor against.
- **Proposed:** the active and expired divider is the outer split: an expired post
  never outranks an active one. The match-count headers group the active posts
  above it; below it, expired posts follow the same order without headers.
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
- **Proposed:** notifications send immediately, and bundle into one email once a
  post gets several in a day.
- **Proposed:** notifications stop when the post expires, and resume when it is
  renewed.

## 9. Freshness

- **Decided:** a post is active for 30 days after it is published, renewed or edited.
  Before it expires, a renewal email renews it in one click, without signing in.
- **Decided:** community posts have a longer period. **Proposed:** 90 days.
- **Decided:** expired posts stay in the feed below the divider (section 4).
- **Proposed:** editing a post renews it.
- **Proposed:** an expired post can be renewed any time from the account page.
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
- **Decided:** an email is sent when a conversation receives a message while it has
  no unread messages for the recipient. Further messages in a conversation that
  already has unread ones send nothing. This is counted per conversation.
- **Decided:** no Discord integration besides sign-in.
- **Proposed:** either side can start a conversation, through the other's post. A
  player without posts can message.
- **Proposed:** conversations continue after their post is deleted, marked "about
  a deleted post". New conversations can't start on a deleted post.
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
│   ShadowFox · Dota 2 player          1 conversation                  │
│                                                                      │
│ Posts you messaged                                                   │
│   The Farlands · Valheim community   Eirik: "Welcome, here's the…"   │
│   Kestrel · Valorant player          You: "Want to duo tonight?"     │
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

## 11. Relaunch and measurement

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

## 12. Data model gaps

The model is redesigned from this brief, not before it. What the brief needs that
the current model lacks:

- **Posts.** Player, group and community posts in place of player profiles, teams,
  team profiles and alerts, with at most one post per player, type and game.
- **Group fields.** Size, open slots, needed roles, a rank range, and an optional
  name, website and Discord server.
- **Community fields.** Name, focus, experience level, Discord invite and website.
- **One free text field** in place of about and ambitions, and a **Looking for**
  value.
- **Card fields.** Which game fields appear on a card.
- **Freshness.** A last renewal or edit time that sorts the feed and decides expiry,
  with a longer period for communities.
- **One feed query** across all three post types, sorted and batched together.
- **Matching.** How many fields of a post match a description or another post
  (7.2), per pair of post types, including rank closeness from the game's ordered
  rank options; usable as a sort key and as a cursor for Load more, and degrading
  to plain recency when the description is empty.
- **Match notifications.** Fired on publish and on renewal after expiry, for the
  owners of posts it fits, with a last sent time for bundling.
- **Messaging.** Conversations tied to a post, messages, unread state, blocks and
  reports.
- **Notifications.** An on-site notification list and badge.
- **Contact preference** on posts.
- **Events** for measurement.

## 13. Not covered yet

- Account page, and editing account details.
- Post pages: what opens from a card, and what crawlers see.
- Home page and header.
- Game pages beyond the feed.
- Notification list.
- Competitions and leagues.
- Design system and visual language.

## 14. Open questions to settle next

1. **Ads.** The current feed injects billboard, leaderboard and mobile units
   between profiles, and every layout in this brief is quietly ad-free. Where do
   ads sit in a feed whose items vary in height and carry match headers? Does the
   description bar or an expanded card ever make room for one? Do post pages, the
   creation flow and the inbox carry ads at all?
2. **Your own posts, and posts you've handled.** A description that comes from your
   own post matches that post best. Do your own posts drop out of the feed? What
   about posts you have already messaged, or people you have blocked: hidden,
   marked, or left alone?
3. **Closing a post that worked.** A group that fills its last slot can only delete
   the post or let it expire. A "We found everyone" action would close the post
   honestly and give the clearest signal the site works. What does it do to the
   post and to open conversations, and does it ask "Did you find them here?"
4. **Mobile.** The description bar, the match-count headers, the expanding card and
   the side-by-side live preview in creation all assume a wide screen. What does
   each become on a phone?

## 15. Out of scope

Decided against for now, listed so they aren't reopened by accident:

- **Reply rate**, and any ranking or badges derived from it.
- **Discord integration beyond sign-in**: no bot, no DMs, no invite validation.
- **Rate limits** on messaging or posting, until abuse appears.
- **Per-field weighting** in matching, and ordering within a match group, until
  there is usage data.
- **Per-field "must match" toggles**: nothing is ever excluded by matching.
- **Per-type notification settings**, and a player field for "not interested in
  communities".
- **Competitions and leagues**: wanted, but not in the first release.
- **Community logos**: uploads and moderation.
- **Members, shared inboxes and roles** on groups and communities: they are posts,
  not entities.
