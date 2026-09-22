# Implementation plan

The redesign in `redesign/brief.md` is built as a rewrite of the application on
the existing platform: PureScript on both sides, Jarilo on the server, Halogen
Hooks for every page and component, Postgres, the two compose stacks, Caddy,
renderready for crawlers and Playwright for the suite. `src/TeamTavern/Database/`
holds the model and the catalogue, `redesign/feed/feed.sql` the feed query,
`redesign/import/` the relaunch import,
and `redesign/prototype/` the specification of every screen's behaviour and
styling. `redesign/handover.md` describes the prototypes and stays as their
reference; this file tracks the implementation.

Every step below ends in the same state: `spago build` clean, `npm test` green
with the step's own spec added, the affected page working in the running stack,
and a commit. A session starts by reading the progress list, does the next
unticked step or part of one, ticks what it finished, notes anything it settled
or left open under the step, and commits. Where a step settles something the
brief marks Proposed, the brief's status is updated in the same commit.

## Progress

- [x] 1. Database in place
- [x] 2. Clear the ground
- [ ] 3. Design system in the client
- [ ] 4. Accounts, sessions and the header
- [ ] 5. Game data and the card
- [ ] 6. The feed
- [ ] 7. Post creation
- [ ] 8. Post pages
- [ ] 9. Home page
- [ ] 10. Contact panel and renewal
- [ ] 11. Messaging and the inbox
- [ ] 12. Block and report
- [ ] 13. Fit notifications
- [ ] 14. Expiry, email and the worker
- [ ] 15. Account page
- [ ] 16. Crawlers, sitemap and old paths
- [ ] 17. Ads
- [ ] 18. Phone and accessibility pass
- [ ] 19. Relaunch
- [ ] 20. Cleanup and docs

Steps 1 through 4 are strictly ordered. From 5 on, the order below is the one
that keeps every step testable in the browser with what came before it; 15, 16
and 17 can be pulled earlier if a session prefers.

## Decisions the plan assumes

Stated here so a session doesn't re-derive them. Any can be overturned; if one
is, update this list.

- **Styling.** Sass stays as the compiler, since the build already runs it and
  Dart Sass loads plain CSS. `tokens.css` and `base.css` move into
  `Client/Style/` unchanged. `components.css` is carved up: each component built
  in PureScript takes its section into a `.scss` beside its `.purs`, registered
  in `Main.scss`, and the section is deleted from the monolith, which lives in
  `Client/Style/Components.scss` until it is empty. Class names stay exactly as
  the prototype has them, so the prototype's CSS and the site's agree while both
  exist.
- **Fonts.** Inter is self-hosted (brief 14.2, Decided): the variable `woff2`
  in the Latin, Latin Extended, Cyrillic and Greek subsets under
  `Client/Static/Fonts/`, with an `inter.css` beside it. The prototype's
  `rsms.me` link is a prototype convenience. Roboto and Font Awesome go.
- **Icons.** `redesign/prototype/icons.js` becomes `Client/Icons.purs`, one
  function per icon returning inline SVG through `halogen-svg-elems`, which is
  already a dependency. Discord's mark is one of them.
- **The SQL that is too big to inline.** The feed query and the fit query stay
  `.sql` files, so `bench.sh`, `check.mjs` and `psql` keep running them. Each
  sits beside a `.js` FFI module that imports it as text, and `build-server.sh`
  passes `--loader:.sql=text` to esbuild. Everything else is inline `Query`
  strings as the server does today.
- **Workers.** The period worker of brief 8 runs inside the node process on a
  timer, as the alert check does today, not as a cron container. One process,
  one timer, nothing to schedule outside the stack.
- **A dev-only components page.** `/design` renders every component in every
  state, as `components.html` does, so a component can be checked by eye and
  screenshotted while it is being built. It is not linked, not in the sitemap,
  and is removed in step 20 if it has no further use.
- **Paths.** Proposed here, settled in the step that first needs each:

  | Path | Page |
  | --- | --- |
  | `/` | Home (11.2) |
  | `/games/:handle` | The feed (4) |
  | `/games/:handle/posts/:id` | A post's page (11.1) |
  | `/post` | Post creation, type step (6) |
  | `/post/:type` | Game step |
  | `/games/:handle/post/:type` | The post screen, new or editing the existing post |
  | `/games/:handle/post/:type/live` | Matches, after publishing |
  | `/signup`, `/signin`, `/forgot-password`, `/reset-password` | Accounts |
  | `/confirm-email`, `/renew` | Landings from email, with a nonce in the query |
  | `/messages`, `/messages/:conversation` | Inbox (10) |
  | `/account` with `#emails` and `#blocked` | Account page (11.5) |
  | `/privacy` | Privacy policy, kept from today's site |
  | `/design` | Dev-only components page |

  The API mirrors them under `/api`. The feed is a `POST` since its description
  is a JSON body; a crawler's render calls it with an empty description, so
  nothing changes for prerendering.
- **Analytics.** The relaunch ships without any (brief 12, Decided), so the
  Google tag leaves `index.html` with the old shell. Ads stay (step 17).
- **Splitgate** is not in the new catalogue (the import drops it), so the site
  has ten games and its cover is deleted.
- **What survives from `src/`.** Infrastructure, not application:

  | Keep | Replace | Delete |
  | --- | --- | --- |
  | `Server/Infrastructure/*`, `Server/Session/*`, `Server/Password/*`, the register step's nickname, email and password validation and Discord fetch, `Server/Main.purs` as the wiring | `Server/Player/*` (account is rewritten around the new `player` row) | `Server/Alert`, `Boarding`, `Game`, `Profile`, `Team` |
  | `Client/Main.purs`, `Client/Script/*` except `Analytics` and `ReloadAds`, `Client/Shared/Fetch.purs` and `Slot.purs`, `Client/Snippets/*` | `Client/Router.purs` (new `State`, new pages), `Client/Static/index.html` and `index.prerender.html` | `Client/Pages/*`, `Client/Components/*`, `Client/Style/*`, `Static/Css`, `Static/Fonts`, `Static/Images/Landing` and `Competitions`, the old favicons |
  | `Routes/Session/*`, `Routes/Password/*`, `Shared/Languages.purs`, `Shared/Timezones.purs`, `DiscordStub/*` | `Routes/All.purs`, `Routes/Shared/*` | Every other route, `Shared/Countries.purs`, `Shared/Regions.purs` (the endpoint replaces them) |
  | `stacks/*`, the build scripts, `test-playwright/stack*.ts` | `test-playwright/integration/*`, `stacks/test-seed/players.sql` | `Database/Migrations/*`, `Database/TablesBase.sql` (see step 1) |

## Phase 0: the ground

### 1. Database in place

`Database/` becomes the new model and the test stack boots on it.

- `redesign/schema.sql` becomes `Database/TablesCurrent.sql`. The relaunch is
  a rebuild, not a migration (schema header), so `TablesBase.sql` is the same
  file and `Migrations/` is emptied; the one dated script there applies to the
  old model only.
- `redesign/seed-regions.sql` and `seed-countries.sql` become
  `Database/Seed/Regions.sql` and `Countries.sql`; `redesign/seed/Games/*.sql`
  replaces `Database/Seed/Games/`. `stacks/test-seed/seed.sh` applies them in
  that order.
- `stacks/test-seed/players.sql` is rewritten for posts. It has to give the
  specs enough to drive every later step through the browser: for every game,
  one account with a player post (nickname `<Handle>Tester`, email
  `<handle>@example.com`, password `tester-password`, as today); for Valorant
  also a second account owning a group post and a community post, so the feed
  mixes all three types; and a third account with no post, for the "signed in
  without a post" states. Posts carry answers to the game's fields, hours, a
  timezone and contacts, so every card row has something to show. One seeded
  Valorant post is expired (its `updated` set back), so the divider has
  something under it.
- `redesign/import/` is left as it is; it is the relaunch's, not the stack's.
- Verify: `docker compose ... up` on the test stack seeds without error; the
  handover's `bench.sh` and `check.mjs` still pass against `redesign_import`
  (nothing here changes them, but they prove the seeds and the schema agree).
- Settled here:
  - The expired post belongs to a fourth account, `ExpiredTester`, rather than
    to one of the others, so every type stays above the divider and the owner
    specs of steps 9 and 14 have an expired post whose owner is known. The
    second Valorant account is `GroupTester`, the one without a post
    `NewTester`; all use `tester-password`.
  - A derived player post answers a single field with its middle option, a
    multi field with its first, and every boolean with yes; the contact
    preference is `either`. The group is reached by `message`, the community
    joined by `discord`.
  - `import.sh` reads the schema and seeds from `src/TeamTavern/Database/`,
    the one edit under `redesign/import/`.
  - `npm test` is red from this commit until step 2: the old application does
    not run on the new schema.

### 2. Clear the ground

Delete the old application and leave a skeleton that builds, serves and passes
a minimal suite, so every later step lands on green.

- Delete what the table above says. Keep the server compiling with a wiring
  record that holds only sessions, passwords and a new `viewGames` (step 5
  fills it out); keep the client compiling with a router whose every state
  renders a placeholder.
- New `index.html` and `index.prerender.html`: the tavern shell. Inter,
  `tokens.css`, `base.css`, the SVG favicon from `redesign/logo/favicon.svg`,
  the site's meta tags, the ad script (step 17 places the units), no Google
  tag. `build-client.sh` copies what the new `Static/` holds and nothing else.
- The suite shrinks to what the skeleton can answer: `smoke.spec.ts` keeps its
  three visitors (browser, prerenderer's browser, bot) against the home page
  only, and `games.spec.ts` its cover checks against the home page's grid,
  which the placeholder home page already renders from `viewGames`. Ten games,
  every cover 600x900.
- `stacks/base.Caddyfile` drops the retired-path redirects; step 16 writes the
  new ones.
- Verify: `./build.sh`, `npm test`, `npm run typecheck` all green; the dev
  stack serves the placeholder home page with the covers.
- Settled here:
  - `Server/Player/Domain/` (`Hash`, `Id`, `Nickname`, `Password`) stays, since
    the session cookie and password reset use it; the rest of `Server/Player`
    and every `Routes/Player` route is gone, and step 4 writes registration
    anew. The session and password SQL still addresses the old `player`
    columns; step 4 reworks it, and nothing in the suite signs in until then.
  - `viewGames` is `GET /api/games` with each game's handle and title, in
    title order, which is the catalogue's order.
  - `components.css` is in as `Client/Style/Components.scss` already, since the
    home page's cover grid uses its `.cover-grid` section. Pages the redesign
    has not built render `Client/Pages/Placeholder.purs`, a heading styled by
    `Placeholder.scss`.
  - The router is a Hooks component with one `State` case per path in the
    table above, and sets each page's title and description itself.
  - `/privacy` keeps today's text in `Client/Pages/Privacy.purs`, styled by
    `base.css` alone.
  - Inter is the Google Fonts build of the variable face, upright only, in the
    Latin, Latin Extended, Cyrillic and Greek subsets (each with its `-ext`
    half), under the family name `InterVariable` that `tokens.css` asks for.
  - `logo-512.png`, the favicon rendered at 512 px, is the Apple touch icon and
    the social card image, since neither takes an SVG.
  - The shell's title is "TeamTavern" and its description "Find players and
    groups for your game on TeamTavern. Say who you're looking for and see who
    fits." Step 9 may replace both with the home page's own.
  - The development database still holds the old catalogue, so its home page
    shows csgo and splitgate without covers until the relaunch rehearsal of
    step 19 builds it from the import.

## Phase 1: foundations

### 3. Design system in the client

Every component of brief 14.5 as a Hooks component or a render function, styled
from `components.css`, shown on `/design`.

- `Client/Style/`: `tokens.css`, `base.css`, `Components.scss` (the monolith,
  shrinking), `Main.scss` registering the per-component files as they appear.
- `Client/Icons.purs` from `icons.js`.
- Components, each with its `.scss`, in `Client/Components/`:
  - Button (filled, outlined, plain, destructive), link styles.
  - Overlay: one component with four presentations, modal, side panel,
    full-screen sheet, bottom sheet, and the dropdown; focus trap, Escape,
    click outside, focus returned to the opener, `inert` on the page behind.
    Phone versus desktop by the 640 px breakpoint, read once and on resize.
  - Inputs from `fields.js`: pills (one or many), tokens with an add select,
    select, range picker over ordered options, hours range that crosses
    midnight, count stepper, checkbox, radio group and radio cards, toggle,
    text area with prompts, the "From your account · Change" fact.
  - Toast with Undo, confirmation dialog with counts, tier heading, labelled
    divider, unread badge and dot, definition list.
- `/design` page rendering all of them in every state the prototype's
  `components.html` shows, phone and desktop.
- Verify: `/design` matches `components.html` side by side at 375 px and
  1280 px, by screenshot; keyboard runs through every overlay; `npm test`
  stays green (no spec for `/design`, it is a tool).

### 4. Accounts, sessions and the header

What every later page needs signed in and out.

- Server, reworked onto the new `player` row:
  - `startSession` (password or Discord), `endSession`.
  - `register`: password (email, nickname, password) or Discord (nickname and
    token; the email Discord gives, `email_confirmed` from Discord's
    `verified`, Discord contact filled). Discord id unique; email unique among
    password players only (the partial index).
  - `confirmEmail` by nonce; `resendConfirmation`; the confirmation email.
  - `forgotPassword`, `resetPassword` as today, on the new tables.
  - `viewMe`: what the header shows, nickname, unread conversation count,
    unread notification count, the games the player has posts in. Counts are
    zero until steps 11 and 13 fill them.
  - Session cookies keep today's names and shape, so sessions imported at the
    relaunch keep players signed in.
- Client:
  - Header (11.4) with Games (the cover grid, `coverGridHtml` in `site.js`),
    the inbox icon and bell (counts only; the lists come in 11 and 13), the
    account menu, Sign in and Sign up, the phone row and ☰ menu.
  - Sign up (the one sign-up screen, 6 step 4), Sign in, Forgot password,
    Reset password, the confirm-email landing. Each returns the player where
    they came from (`?back=` or history state).
  - The signed-in state read from the cookies as today.
- Discord stub: unchanged; the spec answers the authorize redirect itself as
  `sign-in.spec.ts` does now.
- Specs: `sign-in.spec.ts` rewritten (password and Discord, sign up and sign
  in, the nickname prompt, sign out landing home); `header.spec.ts` (menus
  open one at a time, Escape, Games grid marks the games posted in, once step
  7 gives a post to mark).

## Phase 2: listings

### 5. Game data and the card

- Server: `viewGames` (catalogue in order, with each game's active post count
  for the header line of brief 4); `viewGame` by handle (fields with `ilk`,
  `ordered`, `slotted`, `applies_to`, `on_card`, options in order; contacts;
  trackers); `viewCountries` (the twelve regions in order and every country
  with its region). Languages and timezones stay in the bundle.
- Client: `Client/Components/Card.purs`, one shell for the three types
  (14.5), driven by field metadata as `toCard` and `renderCard` in `game.js`
  and `prototype.js` are: heading with the group's numbers or the community's
  name, fact line in the settled order (on-card fields, location or regions,
  languages, microphone, ages, then compared extras), the marks on the facts,
  "≠ Rank not given", the cut and Details expansion with its animation,
  freshness, the expired look, the already-messaged mark, the owner's Edit
  and Renew in place of the contact button. Region names cut at the compass
  point; twelve regions read Anywhere; every slot reads Any role. Hours in the
  viewer's timezone and locale (`clock` in `site.js`).
- The card's data type is the feed query's row, shared with the post page and
  the post screen's preview, so one type feeds all three.
- Verify: `/design` shows the fixtures of `fixtures.js` as cards; compared by
  screenshot with `components.html`.

### 6. The feed

- Server: `viewFeed`, `POST /api/games/:handle/feed` with
  `{ description, showing, cursor }`, running `feed.sql` verbatim from
  `Server/Feed/Feed.sql`. The viewer is the session's player or null; `now` is
  the server's. The response is the query's rows, tiers, `more` and cursor.
  `check.mjs` is pointed at the endpoint as well as at `psql`, so the marks are
  proven against the prototype through the API too.
- Client, from `feed.js`:
  - The feed header with the cover and active count.
  - The description bar: the three choices, the line that says which way to
    read the fields, the compared fields for the chosen type with the most
    used first and the rest under More, the phone's full-screen sheet that
    applies on close. Draft in local storage per game and type
    (`tt-draft-<game>-<type>`), which step 7's post screen reads.
  - Publish prompt, the muted line for the viewer's own post of the type, and
    Update post once the description differs (needs step 7 for the button's
    destination; the line and the button appear here, the button's page then).
  - Showing segments for a player; tiers with counts; the divider; Load more
    with the cursor; the empty-feed states.
  - Batches and scroll position kept across Back from a post page: the loaded
    feed lives in a cache keyed by game that the router owns, restored on
    `popstate`, dropped on a fresh navigation.
  - Title and description meta per game.
- Specs: `feed.spec.ts`: the seeded posts appear in activity order with an
  empty description; a description moves the fitting post into Fits you and
  marks its facts; Showing narrows; the expired post sits under the divider;
  Load more is absent with fewer than 21 posts (a second spec case seeds
  nothing extra, so it asserts the button's absence and the tier counts).

### 7. Post creation

- Server:
  - `viewOwnPost` by game and type (the post with its answers, for the edit
    screen and the existing-post check), `createPost`, `updatePost`,
    `deletePost` (returns nothing; the counts come from `viewOwnPost`, which
    carries the conversation count for the delete confirmation).
  - Validation as `Validated` over the brief's rules: a community's name and
    words required, options must belong to the game's fields, a range's ends
    to the field, regions to the twelve, hours with a timezone, `group_size`
    and wanted only on a group. Account facts and contacts given on the screen
    are written to `player` in the same transaction.
  - `created`, `updated` and `renewal_nonce` set on create; `updated` set on
    edit (brief 9: an edit renews).
  - Publishing notifies (brief 8): the hook is here, its query is step 13.
- Client, from `post.js`:
  - Type step, game step (cover grid with "Your post" on a game the player has
    a post of this type in), the existing-post check with Edit it and Delete
    it (with counts).
  - The post screen: every field, the folded account facts with Change and the
    "Applies to all your posts" note, the group's count steppers, ranges,
    regions pills, languages tokens, the game's fields by metadata, hours and
    timezone, words with prompts, the contact question with the game's
    contacts, Discord server and website; the live preview beside it on a
    desktop and behind Preview in the bottom bar below; Sign up with Discord
    saving the draft and returning; the rule line above Publish post.
  - The register step inside the flow, keeping the draft; on sign-in, the
    existing-post check with "update it with what you entered" or discard.
  - Matches: "Your post is live" with the ember flame, the feed's fits for the
    new post (the feed endpoint with the description built from the post), See
    all into the feed.
  - Edit from the home page and Update post from the feed both land on the
    same screen prefilled.
- Specs: `post.spec.ts`: signed out, describe on the feed, Publish post, fill
  the rest, register, land on Matches, find the post in the feed; signed in,
  New post on a game with an existing post shows the check; edit changes the
  card; delete with the count removes it. Both password and Discord sign-up on
  the way.

### 8. Post pages

- Server: `viewPost` by id: the card's data, whether the viewer owns it, has a
  conversation about it, or is blocked either way; `gone` for a missing id.
  The owner's view carries the state, conversation count and reveals.
- Client, from `post-page.js`: the expanded card as the page, the `h1`, the
  type naming the game, the contact button filled (its panel is step 10; until
  then it is disabled with no panel), the way into the feed with the viewer's
  description or the whole feed, Back to posts only with the feed behind, the
  owner's view with See what fits, the expired note, the blocked line, the
  gone page. `noindex` while expired, `renderready-status-code` 404 when gone.
- Specs: `post-page.spec.ts`: opens from a card's name, Back keeps the feed's
  batches (assert on a loaded card that was not in the first paint, once the
  seed has more than 20 posts in one game, else on the scroll position), the
  expired post carries `noindex`, an unknown id is the gone page and a bot gets
  404.

### 9. Home page

- Server: `viewOwnPosts`: the player's posts by game in catalogue order, each
  with its card heading and facts, state (days left or expired since),
  conversation count and unread count, contact reveals.
- Client, from `home.js`: the signed-out page (What are you posting?, the line
  under it, Or browse a game with the grid), the signed-in page with posts by
  game, the state words, Renew outlined in the last week or after, the
  conversation count into the inbox (step 11 gives it a destination), New
  `<game>` post under the last post, the other games. A player without posts
  gets the signed-out page.
- Specs: `home.spec.ts`: signed out shows the type cards and every cover;
  signed in as the seeded owner shows their posts with the right state words;
  Renew moves a post's freshness.

## Phase 3: contact

### 10. Contact panel and renewal

- Server: `revealContacts` by post id, signed in, counting the reveal on the
  post and returning the contacts and join links; `renewPost` by id, signed
  in, setting `updated` and deleting the post's expiry notification.
- Client, from `messaging.js`: the panel as a side panel and full-screen
  sheet, the preference ordering, the contact rows with Copy, Open the invite
  and Visit site, the "or message on TeamTavern" fold, the reply note before
  the first message; signed out, the button leads to sign up and back to the
  open panel. The message box is wired in step 11; here it is present and
  disabled with the note.
- Specs: `contact.spec.ts`: each preference gives its button label and order;
  signed out leads to sign up and back; a reveal shows on the owner's home
  page as Contacts shown once.

### 11. Messaging and the inbox

- Server: `sendMessage` to a post (creating the conversation on the first),
  `viewConversation` (marking the viewer's side read), `viewInbox` (own posts
  with their conversations, then posts messaged, each with its last message
  and unread state), the unread count into `viewMe`. The message email is sent
  in `sendMessage` when the recipient has nothing unread in that conversation
  and their email is confirmed and the switch on; its Renew button carries the
  post's renewal link.
- Client: the message box in the panel with Enter to send on a desktop, the
  thread with the New line and grouped runs, the inbox page with its two
  sections, the open conversation beside the list or on its own screen, the
  expired-post note and Renew, the header's inbox count, the home page's
  conversation count opening the first unread.
- Specs: `messages.spec.ts` with two browser contexts: one player writes on
  the other's post, the owner sees the unread count, the row and the New line,
  replies, the first sees it in the panel and the inbox.

### 12. Block and report

- Server: `block`, `unblock`, `report` (stored, and emailed to the admin
  address from the environment), the blocked list in the account view (step
  15 renders it; here it is the endpoint). The feed query already hides
  blocked players; `viewInbox`, `viewNotifications` and `viewPost` apply the
  same two lookups.
- Client: the ⋯ menu on the panel and the thread, the block confirmation
  saying what it does, the toast with Undo, the report dialog with its four
  reasons and "also block".
- Specs: `block.spec.ts`: after a block the other's post leaves the feed and
  the conversation both inboxes; the post's page shows the line; Undo brings
  it back.

## Phase 4: notifications and email

### 13. Fit notifications

- `Server/Feed/Fits.sql`: for one post, the other active posts in the game it
  fits, judged from each of those posts' owners' seats: their post as the
  description, this post as the one candidate, fit meaning no miss (brief 7.2,
  Decided). Derived from `feed.sql`'s `described`, `answer` and `ranked`,
  with the description built from a post's rows rather than from JSON. Checked
  the way `check.mjs` checks the feed, against `notifyOwnersFitBy` in
  `game.js`.
- Server: on `createPost`, and on `renewPost` or the nonce renewal of an
  expired post, upsert a `fit` notification for every post it fits, blocked
  pairs excluded (`notification_fit_key`); `viewNotifications` grouped by own
  post in the settled order; `markNotificationsRead` for all and for one; the
  unread count into `viewMe`; deleting a post cascades.
- Client, from `site.js`'s notification menu: the bell's dropdown and phone
  screen, rows opening the fitting post's page or the home page, Mark all
  read, the empty state with New post.
- The Matches screen's count and the notifications must agree: both are the
  feed's Fits you from the owner's seat, and the spec asserts the same
  publish produces both.
- Specs: `notifications.spec.ts`: publish a post that fits a seeded one, the
  seeded owner's bell counts it, the row opens the new post's page, Mark all
  read clears it.

### 14. Expiry, email and the worker

- Server worker, on a period read from the environment (an hour in
  production, seconds in `test.env`): posts entering their last week get an
  `expiry` notification (upsert) and a renewal email; every owner with fit or
  expiry notifications created in the period that passed gets one email
  grouped by their posts (brief 8). Switches, `email_confirmed` and a missing
  address are honoured. A community's renewal email asks whether its invite
  still works.
- `renewByNonce`: `GET /renew?nonce=` lands on the client, which calls the
  endpoint, then opens the game's feed with the description built from the
  renewed post under a note that it is active again; works signed out.
- Emails as HTML with the site's palette, one module each: confirmation,
  password reset, match digest, renewal, message, report. Every player email
  carries the unsubscribe link to `/account#emails`.
- Test stack: a `mail` service like `discord`, `DiscordStub`'s sibling, that
  accepts SendGrid's mail send request and serves the captured emails as a
  page. The node container reaches it through a `SENDGRID_API_URL` that only
  `test.env` sets and the server passes to the SendGrid client as its base
  URL, so a spec reads an email by opening that page in the browser, as a
  player would open their inbox, and clicks the link it carries. Production's
  configuration is untouched.
- Specs: `email.spec.ts`: a message sends one email and a second message none;
  the renewal email's link renews signed out and lands on the feed; the
  confirmation link confirms; a switch off sends nothing.

### 15. Account page

- Server: `viewAccount`, `updateFacts` (nickname, birthday, country,
  languages, timezone, every contact kind), `updateEmail` (back to
  unconfirmed, sends the link), `updateSwitches`, `switchToDiscord` and
  `switchToPassword` with the conflict refusals, `deleteAccount`.
- Client, from `account.js`: the two sections, the joint Edit and Save changes,
  the email row's three states with Send again, the sign-in row and its two
  moves, the switches saying what they send and that nothing is sent while
  unconfirmed, the blocked list with Unblock, Delete account with the counts,
  `#emails` and `#blocked` landings, signed out sent to sign in and back.
- Specs: `account.spec.ts`: a changed location shows on the owner's post in
  the feed and on its page; a changed nickname shows in the other player's
  inbox; a changed email shows unconfirmed; delete lands home signed out and
  the posts are gone from the feed.

## Phase 5: launch

### 16. Crawlers, sitemap and old paths

- `sitemap.xml` served by the node process: the home page, every game's feed,
  every active post's page; expired posts left out. Caddy hands `/sitemap.xml`
  to node beside `/api/*`. `robots.txt` names it.
- The smoke spec's three visitors on every page kind: 200 for the feed, a
  post, the home page; 404 for a gone post and an unknown path; 503 with the
  API down; the stylesheets and cover images a bot's page names are served;
  `noindex` on an expired post's render.
- Caddy redirects for the old paths worth keeping: `/games/:handle/players`
  and `/teams` to `/games/:handle`; everything else old is a 404.
- Discord: the new redirect URIs (`/signup`, `/signin`, the post screen) are
  registered on the Discord app for the dev stack, and the manual check in
  `CLAUDE.md` run.

### 17. Ads

Brief 15: side rails from 1024 px on the feed and the post page, a bottom
sticky on a phone, using the Venatus units the old site had. Nothing the brief
fixes moves for them: the feed stays one 720 px column. `reloadAds` returns
here if the units need it on navigation.

### 18. Phone and accessibility pass

Every screen at 375 px against the prototype's screenshots: the description
sheet, the panel, Games, notifications, the account sheet, the post screen's
bottom bar and preview sheet, the inbox thread on its own screen, the card's
footer rows. Focus order and trapping in every overlay, reduced motion on the
card's expansion, contrast of every token on every surface it is used on. A
`screenshots.mjs` beside the suite, like the prototype's, so the pass repeats.

### 19. Relaunch

- Rehearse on a copy: `redesign/import/import.sh` against the newest dump;
  read the drop report; run the site on the result in the dev stack and click
  through every page as an imported player (a password player, a Discord
  player, a team owner whose team became a group post).
- Runbook, written in this step and kept under `redesign/relaunch.md`:
  backup; stop node; build the new database from the dump with the import;
  rename it into place; deploy the new bundles; start node; the Discord
  redirect URIs on the production app; the manual Discord check; watch
  `docker logs node` for the first worker period.
- After it holds: `TablesBase.sql` and `TablesCurrent.sql` are already the
  same file; `redesign/import/` is deleted, since it addresses rows only the
  old production had.
- The relaunch email to existing players (brief 12, Open) is decided here,
  not before.

### 20. Cleanup and docs

- `redesign/prototype/` is deleted once every screen is live and compared;
  the brief, the logo, this plan and the relaunch runbook stay. The handover
  is rewritten as a short note on where the prototype's knowledge went, or
  deleted if the brief and the code carry all of it.
- `CLAUDE.md` rewritten for the new layout: the request flow and conventions
  still hold, the area names, the database section (no migrations until the
  first post-relaunch one), the test seed's accounts, the mail stub, the
  worker, the `.sql` text modules, the `/design` page if it stays.
- `Components.scss` is empty and deleted; `/design` is kept or removed.
- The memory notes for this project are updated to say the relaunch shipped.

## What is not in the plan

Out of scope by the brief (16), and not to be started by accident: analytics,
competitions, community logos, rate limits, per-field weighting, near-miss
notifications, the nearest-region sort, a light theme, a "We found everyone"
action. The feed's prefill from the viewer's own post on a first visit (brief
7.1, Proposed; handover "Not yet prototyped") is the one Proposed item with no
prototype; it is built in step 6 as the brief describes it, and the brief's
status is updated with what it turned out to need.
