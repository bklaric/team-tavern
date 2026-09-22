# Handover: redesign prototypes

Read `redesign/brief.md` first; it is the source of truth, and its
Decided/Proposed/Open statuses are kept current as things settle.
`src/TeamTavern/Database/TablesCurrent.sql` is the data model written from it,
with `Seed/Regions.sql` and `Seed/Countries.sql` beside it; the test stack
builds on all three.

## The feed query

`redesign/feed/feed.sql` is the one query behind a feed batch (brief 4, 7), in
the positional `$n` form a Jarilo handler sends, ready to move into a server
handler. The description is one `jsonb` parameter keyed by the game's field and
option keys, as the prototype's draft is; the query's header documents it and
the other five parameters. It returns 20 posts with their card data, the marks
each card shows, the tier counts, whether more follow, and a cursor for the
next batch, passed back as it came.

- **It matches the prototype.** `redesign/feed/check.mjs` runs `compare` from
  `game.js` in a browser and diffs it with the query's marks, misses and
  compared counts over every sampled post of four Valorant descriptions (player,
  group, community) and one League one: none differ. Run it after changing
  either side, with the samples freshly exported.
- **It holds up on the dump.** `redesign/feed/bench.sh` times it against
  `redesign_import`. On Valorant, the largest feed (14.9k posts, 27 active), a
  first batch takes 30–45 ms, about what today's player listing takes on the
  same machine (26 ms); a batch that reaches the expired posts 90–200 ms, since
  it ranks every post in the game. The Docker VM here counts a million rows
  several times slower than a server does, so compare with a baseline measured
  alongside, not with absolute times.
- **What keeps it fast**, each explained where it is in the query: expired posts
  are compared only once a batch reaches them, which the cursor's count of posts
  shown decides; a post's answers to the game fields fold into two bitmasks, the
  fields answered and the fields that fit; timezones are converted once each;
  ages are compared as birthday bounds; and the marks and card are built for the
  batch alone.
- **What it leaves out.** The count of expired posts, since counting them would
  rank them, and the divider shows none; and the header's active count, a
  game-level figure for its own query.
- `redesign/import/import.sh` now ends with `vacuum analyze`. Without statistics
  the planner misjudged the answers by three orders of magnitude and the query
  took seconds.

## Where things stand

- **Design system** (brief 14): dark by default, tavern palette, Inter, Lucide.
  `redesign/prototype/components.html` is the sheet showing every component and state.
- **Feed** (brief 4, 5, 7): `feed.html?game=valorant` runs on real posts from the
  12 September dump: the description bar, matching, tiers, the expired divider,
  Load more and the phone sheet. The bar is headed "Tell us about you" and says
  posts that fit come first; under the three choices a line says which way to
  read the fields, since it changes with the choice. One card question the brief
  answers twice: a community says what it runs on through the game's own fields,
  so Valheim's server type leads its card as brief 5.3's example does, and leads
  a player's and a group's there too, since a game field is the same field on
  all three post types (brief 5), where brief 5.4's table keeps every game field
  but rank, roles and Looking for behind Details.
- **Post creation** (brief 6): `post.html` runs the whole flow: Type, Game, Post,
  Register or sign in, Matches, plus the existing-post check, delete with its
  counts, the sign-in conflict, and Sign up with Discord. The feed's **Publish
  post** and **New post** lead into it. Settled while building it:
  - Below a desktop the preview opens on demand from a bottom bar that also holds
    Publish post. The "above the fields" variant is still behind the prototype bar.
  - On a phone a card's owner line sits on its own row above the buttons.
  - An account fact or contact the account already holds shows folded, "Croatia ·
    From your account · Change"; changing it shows "Applies to all your posts"
    when the player has other posts.
  - Only a community's name and words are required, tagged **Required**; nothing
    carries an optional tag. Every field of the post is on the screen, with
    nothing behind a click: what the card keeps behind Details doesn't shape the
    form. Trackers come from the game account ID through the game's templates.
  - Multi-option fields with few options are pills, the twelve regions among
    them; languages are tokens with an "Add a language" select. A group is asked
    how many it is and how many more it wants in one row of count steppers,
    "[3] players, want [2] to [2] more", which the card heading reads back.
  - The cover grid has no captions: four tiles across on a desktop, three on a
    phone, the title as each image's alt text. A game the player already has a
    post of this type in says "Your post" on its cover. No search yet (brief 14.3).
  - "Your post is live" carries the ember flame, not a green check: green means
    only a field that fits.
- **Game fields** (brief 5, 7.2): every game in the catalogue has a sample, and
  the pages read a game's fields from it with their `ilk`, `ordered`, `slotted`,
  `applies_to` and `on_card` (`src/TeamTavern/Database/TablesCurrent.sql`); nothing in the prototype
  names a field. A post's answers use the keys the post screen writes:
  `field:<key>` for the options chosen or a yes, `range:<key>` for an ordered
  field on a group or community. Settled while building it:
  - The fields `on_card` marks lead the card in the game's order, then the
    location or regions, languages, microphone and ages. The rest wait behind
    Details, and join the end of the fact line while compared, as the hours do.
    That puts platform and Looking for before the location, where the brief's
    diagrams have them after the languages.
  - Where a card leads with more than one ladder, which is only Overwatch, each
    rank names itself: "Tank rank Emerald 3".
  - A yes-or-no field is a checkbox on the post screen and a toggle in the bar.
    A player's reads as the field, "In-game leader", asked as "I can be an
    in-game leader"; a group's as a need, "Needs an in-game leader". A viewer
    who ticks it sees "Not an in-game leader" or "Doesn't need an in-game
    leader" marked on a post that didn't.
  - A group asks a range of every ordered field, "Tank rank range" to "6v6 rank
    range", and its slotted field as "Roles you need".
  - A slotted field offers **Any role** (Any position, Any class), which ticks
    every option; a card and the bar's chip read every option back as "Any
    role", as twelve regions read "Anywhere".
  - Platform is a field like any other, so a game whose seed has none shows
    none.
- **Matching** (brief 7.2): `compare` in `game.js` takes each field that both
  post types are asked and compares it by what it is. An ordered field is near
  between two players and inside the range against a group; a slotted one is
  covered between two players and filled against a group; a boolean fits on
  agreement and only where the viewer ticked it; any other field fits on a
  shared option. The account's facts keep their own rules: a group and a
  community ask for the players they want in the same ones, so the ages, the
  microphone and the online hours are compared whichever of the two a player
  meets, and whichever way round. A microphone is compared only where the viewer
  gave one, since a player saying they use one and a group asking for one are
  both an answer, while silence is not.
- **Rank closeness** (brief 7.2, Decided): two players' ranks are near within a
  tier's width of each other, read from the labels until usage shows the seed
  should state it. A tier is the options whose labels differ only in a trailing
  division, and the game's commonest tier is how many steps that is: 3 in Valorant, 4 in League and Apex, 5 in Overwatch, Dota,
  HotS and Siege. A ladder without divisions, such as TF2's divisions, Faceit
  levels or CS2's Premier rating, counts one step. **Near rank** in the
  prototype bar switches the feed to one step everywhere, to compare.
- **Regions and countries** (brief 5, 7.2): the twelve regions and the 229
  countries are in `fields.js`, written from `src/TeamTavern/Database/Seed/Regions.sql`
  and `Seed/Countries.sql`. A player gives a country, a group or community
  gives regions, and a country is compared through the region it is in. Settled
  while writing them in:
  - A card names a region by its own name, cut at the compass point:
    "N. America", "SE Asia". No code fits twelve, since SA is South America and
    South Asia at once, and a region has to be legible to the player picking one
    (brief 5). The brief's card diagrams still abbreviate Europe to EU.
  - A post that names all twelve reads "Anywhere" rather than spending the fact
    line on the list.
  - A group or community picks from twelve pills; the bar's chip counts the
    rest off, "Europe, Middle East +3".
- **Contact and messaging** (brief 5.6, 10): every card's contact button opens
  the contact panel in `feed.html` and on the Matches screen, and
  `messages.html` is the inbox. Signed out, the button leads to the sign-up
  screen and back to the open panel. Settled while building it, Proposed in the
  brief unless marked:
  - On a desktop the panel is a side panel, not a modal (Decided). On a phone
    it is full-screen.
  - Off-site preferences put the contacts first and outline **Send**; the others
    put the message box first. A conversation under way always comes first.
  - The inbox lists each of the player's own posts with its conversations under
    it, rather than a row per post to open. An expired one offers **Renew**.
  - A conversation about your post is headed with the other player and their
    post's facts; one about someone else's post with that post, its contacts
    folded away.
  - Block and report sit in a ⋯ menu on the panel and the conversation; a block
    is undone from its toast.
  - The prototype's accounts post too: Kestrel's Night Owls is in the Valorant
    feed, with **Edit** and **Renew** for Kestrel and a contact button for
    everyone else, and whatever a viewer publishes joins its game's feed.
- **Post pages** (brief 11.1): `post-page.html?game=valorant&id=<post id>`, the
  page every card's name opens, the home page's posts among them. The post is an
  expanded card, whose contact button opens the contact panel as it does in the
  feed, and below it a strip into the game's feed. An expired post's page really carries `<meta name="robots"
  content="noindex">`, and a stand-in note says the sitemap leaves it out;
  renewing takes both away. An unknown or deleted id gets the gone page, with a
  stand-in note for the 404. Settled while building it, Proposed in the brief:
  - The card is the page: its name is the `h1`, it is expanded with no
    **Details** button, and its type names the game, "Valorant player".
  - No match marks on it: a description is personal and isn't shared, so what
    fits is the feed's business, and the strip below the post is where the
    viewer's own description comes in.
  - The contact button is the page's one filled button, where a card's is
    outlined in the feed.
  - The owner sees their own view: **Edit** and **Renew**, the state, the
    conversations and the contact reveals, and **See what fits**, which takes
    the description from this post, where a visitor gets the feed strip.
  - **Back to Valorant posts** shows only while the feed is the page behind,
    and calls the browser's Back, which is what keeps the batches. The feed
    keeps what it had loaded, and its scroll position, for the tab.
  - A blocked player's post keeps its page without its contact button, under a
    line saying why.
- **Home page** (brief 11.2): `home.html`, where the logo leads. Signed out, or
  signed in without a post, it is "What are you posting?" with the type cards,
  then the cover grid into each game's feed. With posts it is the player's
  posts by game, each with its state, conversations and contact reveals, and
  **See what fits**, **Edit** and **Renew**; the other games follow. Settled
  while building it, Proposed in the brief:
  - A post there is its card's heading and facts without the owner's words.
  - The state counts forward for the owner: active for so many more days,
    "Expires in 3 days" in its last week, then expired, with what that means.
  - Renew is outlined only in the last week or once expired; the page has no
    filled button.
  - The conversation count opens the inbox on the post's first unread
    conversation, in the inbox's order, else on its first conversation.
  - A game's cover stands beside its posts without a title, top edges level;
    on a phone the name sits beside the shrunken cover. Games keep the
    catalogue's order, so renewing doesn't move a post, and a game without all
    three of the player's types offers a new post for it below the last.
  - A feed whose description comes from the viewer's own post of that type
    says so in a muted line, and offers **Update post** once the description
    differs, never Publish post.
- **Header** (brief 11.4): the same header on every page, and the menus it
  opens. The logo leads home, **New post** is its one prominent button,
  outlined, and **Games** opens the cover grid, each cover opening that game's
  feed. Signed in, the inbox icon carries its unread count and opens the inbox,
  the bell carries its count, and the account menu holds the nickname, **Your
  posts**, **Account** and **Sign out**. Signed out it is Sign in and Sign up,
  which a ☰ menu holds on a phone. Settled while building it:
  - Games holds the whole catalogue at once, in about two rows, on covers
    smaller than the home page's: nothing is scrolled or searched for.
  - A game's mark reads "Your post", or "Your posts" for several. The header
    knows no post type, so it marks the game, where posting's grid marks a game
    the player already has a post of that type in.
  - New post keeps its label signed out on a phone and drops to the plus signed
    in, where the two counts and the account button share the row with it.
  - One menu is open at a time; a click outside or Escape closes it, Escape
    gives the focus back to the button that opened it, and opening one takes
    the focus into it.
  - On a phone Games and notifications are full-screen, like the feed's
    description sheet, and the account menu is a sheet from the bottom, as tall
    as the four rows it holds.
  - Sign out lands on the home page.
  - The cover grid is one function, `coverGridHtml` in `site.js`, which the
    header, the home page and posting's game step all call.
- **Notifications** (brief 8, 11.3): the bell's list, grouped by the player's
  own posts. A post that fits opens its own page, a post of yours about to
  expire opens your posts, unread rows carry a dot and **Mark all read** clears
  them, and the bell's count is how many are unread. The store stands in for the
  server like the conversations': it outlives switching who is viewing, and
  **Start over** forgets it. Settled while building it, Proposed in the brief
  unless marked:
  - The post with the newest notification leads; inside a post its own expiry
    comes first, then what fits it, newest first.
  - A row about a post that fits carries the time that post was published, in
    the same words a card's freshness uses. The expiry row reads the post's
    state now, in the home page's words, and carries no time of its own:
    renewing the post takes it away, and there is never a second one for a post.
  - A notification lives under the post it is about, so deleting that post takes
    it with it (Decided), as deleting a post takes its conversations. Nothing
    else removes one: a row whose post has since expired or been deleted still
    opens that post's page, which says which it is.
  - The list scrolls inside the dropdown; there is no notifications page, and
    the store keeps the newest 50 a player has.
  - Publishing a post really notifies the owners it fits: `notifyOwnersFitBy`
    in `game.js` compares it with the other accounts' posts in the game the way
    the feed compares it with their description. Only publishing does, and only
    for posts that are active and not blocked either way; renewal after expiry
    (brief 8) is seeded, not live, since the home page has no game data.
- **Account page** (brief 11.5): `account.html`, where the account menu's
  **Account** leads. **Shown on your posts** holds the nickname, birthday,
  location, languages, timezone and contacts, edited together under one
  **Edit**; **Only you see this** holds the email, the way the player signs in,
  the email switches, the blocked list and **Delete account**. Signed out the page sends the player to
  sign in and comes back. Settled while building it, Proposed in the brief
  unless marked:
  - A change to a fact really reaches every post: the facts each account holds
    live in a store of their own, as its posts and conversations do, so an
    edited location shows on the player's posts in the feed and on their pages,
    and an edited contact in the panels other players open, whoever is viewing.
  - A post keeps the card it was published with (site.js), so the page corrects
    the copy too where the card shows the fact, and the home page agrees with
    the feed.
  - A nickname is how a player is named in the stores, so changing one rewrites
    the conversations, notifications and blocks that name them (`renamePerson`).
  - The blocked list stands in the section rather than behind a button of its
    own (Decided), and a blocked player's post page links to it. Nobody is
    blocked to start with: block someone from a contact panel and the list has
    them.
  - Delete account counts the real posts and conversations before it takes
    them, and lands on the home page signed out. Viewing as that account again
    gives the preset's facts back but none of its posts, which is the prototype
    bar's doing rather than the site's.
  - The email switches keep their state and say in a stand-in toast what they
    would do; nothing here sends email. `account.html#emails` is where an
    email's unsubscribe link lands, and `#blocked` where a blocked player's
    post sends its viewer.
  - The controls the facts are edited with are the post screen's, moved to
    `fields.js` so one set serves both.
  - The email and the way the player signs in are rows of their own (Decided),
    since Discord knows an account by its id and the email is only where the
    site writes. How an account signs in is its own field, `signIn`.
    **Continue with Discord** takes the password's place and leaves the email
    alone, filling the Discord contact only where there is none; **Use a
    password** signs in with the address the account holds, and asks for one
    where it has none.
  - The email row carries where the address stands (brief 6, step 4): confirmed,
    unconfirmed with **Send again**, or missing, which says what the site then
    can't write about. A changed address goes back to unconfirmed, and the
    switches say that nothing is sent while it is.

## Files

| File | What it is |
| --- | --- |
| `tokens.css`, `base.css`, `components.css` | The system: these become the new `Client/Style` |
| `prototype.css` | The prototype bar and stand-ins: not part of the system |
| `prototype.js` | `renderCard(post, marked)`, `renderOwnPost(post)` and the shared helpers (icons, facts, slots) |
| `site.js` | What every page shares: the clock, games and the cover grid, the type cards, the accounts the bar switches between, the store that stands in for the server (conversations, notifications, blocks, reports, contact reveals, email switches and the facts each account holds), where the feed and a post's page live, what an owner is told about a post, the header with its Games, notification and account menus, the prototype bar and toasts |
| `fields.js` | What the post screen and the account page both ask for: the twelve regions, the countries and the region each is in, the languages, the game accounts, and the control each kind of field draws, reads back and shows as a value |
| `game.js` | One game's data and what the pages derive from it: game fields, posts (the dump's and the accounts'), made-up contacts, trackers, rank closeness, `compare(post, type, description)`, `toCard`, and the editors for a rank, age or hours range |
| `feed.js` | Feed only: the description, tiers, the bar and the phone sheet |
| `post.js` | Post creation, and the site's one sign-up screen |
| `messaging.js` | The contact panel, the conversation thread, inbox rows, block and report |
| `messages.js` | The inbox page |
| `home.js` | The home page |
| `post-page.js` | A post's own page: the post, what its owner is told, and the way into the game's feed |
| `account.js` | The account page: the facts the posts show, and what only the player sees |
| `conversations.js` | Handwritten conversations the store starts with; nobody in them is from the dump |
| `notifications.js` | Handwritten notifications the store starts with; the posts that fit are the dump's, apart from Dota 2's, which are made up |
| `fixtures.js` | Handwritten posts for the sheet; production content stays out of the repo |
| `export-sample.sh` + `.sql` | Writes `data/<handle>.js` (git-ignored) from `redesign_import`, the dump in the new schema that `redesign/import/import.sh` builds: the game's fields with their metadata, trackers, and posts with their options, ranges and flags. Every game is exported |
| `screenshot.mjs` | Phone and desktop screenshots; `--parts`, `--sections`, `--locale=en-US` |

## Using the pages

- The dashed bar above the header is the prototype's, not the design's. It picks
  who is viewing: signed out; Kestrel, who has a Valorant group post (so Valorant +
  group shows the existing-post check); or Vex, before a first post. Switching
  clears drafts. `?as=out|kestrel|vex` does the same from a URL.
- Discord is stood in for by a dialog: a new player (Mira), whose address it
  also picks, or Kestrel's account.
- Sign in with any email and password signs in as Kestrel.
- Drafts live in local storage per game and type (`tt-draft-<game>-<type>`).
- The bar is on every page. Conversations, notifications, blocks, reports, what
  each account has published and the facts each account holds outlive switching
  who is viewing, as a server's would, so Vex can message Night Owls and Kestrel then finds it unread
  in the inbox. **Start over** forgets them too, which is also what puts Vex
  back before a first post.
- With a conversation open, **Reply as** the other side answers it. Dashed
  "Stand-in" toasts say what the site does offstage, such as whether a message
  sends an email (brief 10).
- Kestrel has a post in each state: Night Owls, active; a Valheim player post in
  its last week; and an expired Dota 2 player post. Each post
  keeps a copy of its card from when it was published, since the home page
  spans games and `game.js` holds one.
- Contact reveals start from made-up counts, and every contact panel opened
  on a post with contacts adds one, so Vex opening Night Owls' panel shows on
  Kestrel's home page.
- A post's page opens from any card's name, in the feed, on the Matches screen
  and on the home page. To reach one directly, `post-page.html?game=valorant&id=<id>`
  takes any id the game's sample holds, `kestrel-valorant-group` for Night Owls,
  and anything else, such as `id=gone`, gets the deleted post's page.
- Load more on the feed, open a post from a card's name and press Back: the
  batches and the scroll position are where they were. Switching who is viewing,
  or **Start over**, forgets that along with the rest.
- **Games** in the header opens every game: as a dropdown on a desktop, full
  screen on a phone. Signed in, the games you have posted in carry a mark, so
  Kestrel sees one on Valorant, Valheim and Dota 2 and Vex sees none. A click
  outside or Escape closes any of the header's menus, and only one opens at a
  time. The account menu is a dropdown from the avatar, and a sheet from the
  bottom of a phone.
- The bell opens the notification list. Kestrel's holds something of every
  kind: three Valorant players that fit Night Owls, two unread, one of them a
  post that has since expired; the Valheim post's own "Expires in 3 days"; and
  under the expired Dota 2 post its "Expired 3 weeks ago" with a group and a
  community that fit it. A row opens what it is about, so the Valorant ones
  reach real pages and the made-up Dota 2 ones the deleted post's page. Vex has no posts, so their list is the empty state, and
  signed out there is no bell at all.
- To make a notification happen, publish a post that fits one of Kestrel's:
  as Vex, describe yourself on the Valorant feed as a Diamond 1 Sentinel on PC
  who can be the in-game leader, plays Ranked, is in Germany, speaks English,
  uses a microphone and is on from 21:00, publish that with a birthday, then switch to Kestrel, where the bell says "Vex fits · just now" under
  Night Owls and the row opens Vex's post. Editing or renewing an active post
  notifies nobody (brief 8).
- **Account** in the account menu opens the account page. Edit changes the facts
  every post of the player's shows: set Kestrel's location to Germany and their
  Valheim post says Germany in the feed, on its page and on the home page; change
  the Discord and the contact panel Vex opens on Night Owls shows the new one.
  Changing the nickname carries through the inbox, the notification list and the
  blocked lists that name it.
- Nobody is blocked to start with. Block someone from a contact panel's ⋯ menu
  and their posts and your conversation go; the account page lists them, and
  **Unblock** brings both back. A blocked player's post page links there.
- **Delete account** counts what goes first: Kestrel's 3 posts and 7
  conversations. It lands on the home page signed out, and the conversations are
  gone from the other players' inboxes too. Viewing as Kestrel again gives the
  preset's facts back but none of its posts, since the bar builds an account
  where the site has none; **Start over** puts everything back.
- The email switches keep their state, and a dashed toast says what the site
  would do; nothing here sends email. `account.html#emails` is where an email's
  unsubscribe link lands.
- Sign-in's **Change** offers Kestrel **Continue with Discord**, which leaves
  the email as it is; **Use a password** goes back, asking only for a password.
  Changing the email leaves it unconfirmed: **Send again** raises a stand-in
  toast whose **Click the link** stands in for clicking the one in the email,
  and confirms it.
- The Discord dialog says what Mira's Discord sends: a verified address, one
  Discord has not verified, or none. The first is confirmed at once; the second
  arrives unconfirmed, as a typed address does; the third leaves her with no
  address, which the account page asks for. Registering with Discord fills her
  Discord contact either way.
- **Sign out** lands on the home page, signed out. The prototype bar's Viewing
  as does the same and more, since it also picks who signs back in.
- Kestrel's inbox has something of every kind: an unread reply, a conversation
  in Russian, one on an expired Dota 2 post, a community with join links, and an
  older post nobody answered. The dump records only whether a contact exists,
  so handles are made up from the name.

## Practicalities

- The dev stack's `postgres` container holds the dump; the export needs it running.
- Screenshot with `"$(volta which node)" redesign/prototype/screenshot.mjs ...`. The
  plain `node` shim passes arguments through `cmd.exe`, which splits URLs at `&`.
- Bash heredocs containing apostrophes fail in this environment. Write one-off
  scripts with the Write tool into the git-ignored `screenshots/` folder, run them,
  then delete them.
- The brief's ASCII boxes align by UTF-16 length; the 🎤 counts as two, as it
  renders. Check widths when editing a diagram.

## Not yet prototyped

Every screen the brief describes is prototyped. What is left of it:

- The prototype's feed doesn't prefill the description from the viewer's post in
  the game on a first visit (brief 7.1); it takes one only through See what fits,
  Matches or a renewal link. The site's feed does.
- The ads of section 15 have no place in any layout yet.

## Stand-ins

What the prototype says it does rather than doing, all of it marked on the page
or in a dashed toast:

- **Email.** Nothing here sends any, so the account page's switches only keep
  their state, and every place an email would go says so: a message that starts
  a conversation, a renewal, a match, an address waiting to be confirmed. A
  message to a player whose address isn't confirmed says no email goes out.
- **Discord.** A dialog stands in for leaving the site: it signs in a new
  player, Mira, or Kestrel, whichever way Kestrel's account signs in, and says
  what Mira's Discord sends. Moving an account to Discord on the account page
  leaves nowhere either, and a Discord already signing in to another account is
  only a note. A later Discord sign-in filling an email the account lacks is in
  the brief and not in the prototype, which signs in nobody twice.
- **Renewal after expiry.** Publishing a post really notifies the owners it fits
  (brief 8); renewing an expired one doesn't, since the home page holds no
  game data. Those notifications are seeded instead.
- **What a crawler or a mail server gets.** An expired post's page carries a
  real `noindex`, but the sitemap leaving it out and a deleted post's 404 are
  notes on the page.
- **Counts that a server would keep.** Contact reveals start from made-up
  numbers, and every panel opened on a post with contacts adds one.
- **A group's numbers.** Today's teams record neither how many they are nor how
  many more they want, so a group from the dump has both made up from its id.
- **Dota 2's notifications.** The group and community that fit Kestrel's Dota 2
  post are made up, as the conversations are, so their rows open the deleted
  post's page.

Two things the prototype does deliberately, which read as gaps until they are
read twice: a conversation keeps the post's facts and contacts as it saw them,
so a contact changed on the account page shows in the feed's panels but not in
an old conversation; and the prototype bar's Viewing as builds an account from a
preset, so it brings a deleted one back, without the posts it had.
