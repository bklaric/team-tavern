# Implementation plan

The redesign in `redesign/brief.md` is built as a rewrite of the application on
the existing platform: PureScript on both sides, Jarilo on the server, Halogen
Hooks for every page and component, Postgres, the two compose stacks, Caddy,
renderready for crawlers and Playwright for the suite. `src/TeamTavern/Database/`
holds the model and the catalogue, `src/TeamTavern/Server/Feed/` the feed and
fit queries with `redesign/feed/` checking them, `redesign/import/` the
relaunch import,
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
- [x] 3. Design system in the client
- [x] 4. Accounts, sessions and the header
- [x] 5. Game data and the card
- [x] 6. The feed
- [x] 7. Post creation
- [x] 8. Post pages
- [x] 9. Home page
- [x] 10. Contact panel and renewal
- [x] 11. Messaging and the inbox
- [x] 12. Block and report
- [x] 13. Fit notifications
- [x] 14. Expiry, email and the worker
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
  one account with a player post (nickname `<Handle>Tester` with the handle's
  hyphens dropped, email `<handle>@example.com`, password `tester-password`, as today); for Valorant
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
- Settled here:
  - A component is a render function taking its state and handlers, so the
    page owning the state owns the actions. Only Tokens (focus goes to the add
    select once a token's button is gone) and the toast (its timer, as the
    `useToast` hook) keep anything of their own.
  - The overlay is `useOverlay` plus the `overlay` render function, not a child
    component, so its content keeps the caller's actions. `Client/Script/Overlay.js`
    does the holding: the modal four make everything beside the overlay's layer
    and its ancestors `inert`, lock the scroll with a count so nested overlays
    release only their own, and keep Tab inside; Escape closes only the newest
    overlay; focus returns to the opener unless the player put it elsewhere. A
    dropdown is not modal, closes on a press outside itself and its opener, and
    takes its class and role from the caller (`menu`, later `header-dropdown`).
  - Callers pick the presentation with `usePhone` (below 640 px); the modal and
    side panel also go full screen on a phone through the CSS alone.
  - Icons set `class` as an attribute: `HP.class_` writes `className`, which an
    SVG element refuses.
  - The hours range's note is its field's hint (`hoursHint`); an age range is two
    number inputs. The type chooser's link cards (`typeCardsHtml`) are posting's
    and the home page's, built in step 7 or 9; `choices` here are the radio cards.
  - The shells carry `<meta id="meta-robots">`, set by the router: `noindex` on
    `/design`, the same switch step 8 needs for an expired post.
  - Left in `Components.scss`: everything a later component owns (cards, header,
    feed bar, popover, contact panel, inbox, notifications, cover grid). The
    moved sections are registered in `Main.scss` in their old order, before the
    monolith, so its overrides still win.

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
  - The session cookie keeps today's name and token, so sessions imported at
    the relaunch keep players signed in. It is the only cookie, `HttpOnly`,
    and only the server reads it.
- Client:
  - Header (11.4) with Games (the cover grid, `coverGridHtml` in `site.js`),
    the inbox icon and bell (counts only; the lists come in 11 and 13), the
    account menu, Sign in and Sign up, the phone row and ☰ menu.
  - Sign up (the one sign-up screen, 6 step 4), Sign in, Forgot password,
    Reset password, the confirm-email landing. Each returns the player where
    they came from (`?back=` or history state).
  - The signed-in state asked of `viewMe` (`fetchMe`), the cookie being out
    of the page's reach.
- Discord stub: unchanged; the spec answers the authorize redirect itself as
  `sign-in.spec.ts` does now.
- Specs: `sign-in.spec.ts` rewritten (password and Discord, sign up and sign
  in, the nickname prompt, sign out landing home); `header.spec.ts` (menus
  open one at a time, Escape, Games grid marks the games posted in, once step
  7 gives a post to mark).
- Settled here:
  - Every Discord round trip returns to `/signin`, the redirect URI the Discord
    app already has for each origin, with a random `state` checked on return.
    Where the player was headed rides in session storage (`tt-discord`), so
    step 7's Sign up with Discord passes the post screen's path and keeps its
    draft in local storage as the description bar does. `/signin` signs in a
    player Discord knows; for one it doesn't, `startSession` answers
    `unknownDiscord` with the Discord username, and the page shows the nickname
    prompt, which registers them. `/signup` has no Discord return of its own.
  - The account pages return through `?back=`, a path on the site that is never
    an account page (`Client/Script/Back.purs`); the header's Sign in and Sign up
    carry the page they are opened from.
  - `viewMe` is `GET /api/me`: the nickname, both unread counts (zero until
    steps 11 and 13) and each game the player has posts in with how many. The
    header shows neither the account nor Sign in until `viewMe` first answers,
    and asks again on every navigation, which the router counts so even a link
    to the open page closes the menu. A refused session shows signed out.
  - Registration stores a Discord address that validates whether or not
    Discord verified it, confirmed only if verified, and the Discord tag as
    the username (`name#1234` where Discord still reports a discriminator).
    Sign-in fills a missing email by the same rule. A typed or unverified
    address gets an `email_confirmation` row and the confirmation email; a
    failed send is logged and doesn't fail the request.
  - `confirmEmail` (`POST /api/confirm-email`) uses a link once, doesn't
    expire, works signed out, and confirms only while the player's email is
    still the address it was sent to. `resendConfirmation`
    (`POST /api/confirm-email/resend`) has no page until step 15.
  - Sign-in takes the email or the nickname, as the server always has.
  - Signing out revokes the session row as well as clearing the cookie.
    Signing in or up replaces the session the browser holds, revoking it, so
    none of the account routes refuses a browser that is signed in.
  - A session lasts a year from its last use. `viewMe`, which the header asks
    on every page, renews the cookie for as long.
  - The header's covers come from `Client/Components/CoverGrid.purs`, which the
    home page uses too; the account pages are `Flow.purs` columns
    (`.flow`, `.flow-narrow`, `.flow-lead`, `.form-tight` from `post.html`).
  - Hooks effects hold the component's next render until they finish, so the
    header forks its fetches; a page whose clicks must answer while it loads
    does the same.
  - No spec follows the confirmation or reset link: their nonces reach only the
    node log until step 14's mail stub, whose `email.spec.ts` covers both.

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
- Settled here:
  - `viewGames` carries each game's `active` count. `viewGame` is
    `GET /api/games/:handle`, 404 for an unknown handle, with `shortTitle`,
    `active`, the contact kinds, the trackers (`contact`, `title`, `template`)
    and the fields (`Routes/Shared/Field.purs`) with their options in order.
    `viewCountries` is `GET /api/countries`: the regions in order, and the
    countries by region, then name.
  - The card row is `Routes/Shared/Card.purs`, labelled with `feed.sql`'s own
    column names so the server's `read` decodes a row as it comes. `feed.sql`
    now also returns the group's numbers; `messaged` as the time of the viewer's
    first message, null without one; a player post's trackers, each with the
    owner's account its template takes; and both times as ISO strings, since
    node-pg hands `timestamptz` over as a `Date`. `check.mjs` still agrees and
    `bench.sh` is unchanged within noise.
  - A tracker shows the owner's game account ID to anyone reading the card
    (brief 5.4), without the reveal the contact panel counts.
  - `Client/Components/Card.purs` is a render function over the row, the game
    from `viewGame` and the viewer (`now`, timezone). A player's heading is
    their nickname. Unmarked, a card ignores whatever marks its row carries.
    Marks are keyed by the field's bare key, as `feed.sql` gives them. The
    caller owns the expansion and wraps its change in `toggleCard`
    (`Client/Script/Expand.purs`), which animates the height unless motion is
    reduced.
  - `Script/Ago.purs` is the prototype's `ago`, replacing `LastUpdated.purs`.
    Language codes are `languageCode` in `Shared/Languages.purs`. Region names
    are `Card/Regions.purs`.
  - The card's rules are in `Card.scss`. `.feed-stack` stays in the monolith
    for step 6.
  - `/design`'s cards are `Pages/Design/Cards.purs`, the prototype's fixtures
    as rows over the seeded Valorant and Valheim fields, which the page fetches
    through `viewGame`. Where the prototype names a field the seed doesn't
    have (Agents, Playstyle), the fixture uses one it does.

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
    applies on close. The description in local storage per game
    (`tt-description-<game>`), which step 7's post screen reads into its own
    draft (`tt-draft-<game>-<type>`).
  - Publish prompt, the muted line for the viewer's own post of the type, and
    Update post once the description differs (needs step 7 for the button's
    destination; the line and the button appear here, the button's page then).
  - Showing segments for a player; tiers with counts; the divider; Load more
    with the cursor; the empty-feed states.
  - Batches and scroll position kept across Back from a post page: the loaded
    feed lives in a cache keyed by game that the router owns, restored on
    `popstate`, dropped on a fresh navigation. The browser's own scroll
    restoration runs on `popstate`, before the page has drawn anything, so
    `history.scrollRestoration` goes to `manual` and the site restores the
    position itself once the batches are back.
  - Title and description meta per game.
- Specs: `feed.spec.ts`: the seeded posts appear in activity order with an
  empty description; a description moves the fitting post into Fits you and
  marks its facts; Showing narrows; the expired post sits under the divider;
  Load more is absent with fewer than 21 posts (a second spec case seeds
  nothing extra, so it asserts the button's absence and the tier counts).
- Settled here:
  - `viewFeed` answers `{ posts, tiers, more, cursor }`, the tiers counting
    active posts and the cursor the last row's. An empty `showing` is every
    type, and a group or community is shown players whatever it asks.
  - `Server/Feed/Feed.sql` is `Feed.js`'s text import from its place in `src/`,
    since purs copies only the `.js` into `output/`. The query opens by typing
    its six parameters, which node-pg sends untyped. Its tier counts are
    `bigint`, which node-pg gives as strings. `bench.sh` is unchanged within
    noise; `check.mjs --api <origin>` pages the endpoint along its cursor and
    agrees with the prototype on everything but ages, which the samples carry
    as of the dump's date, so that way leaves them to the psql way.
  - One description shape everywhere, the query's, as
    `Routes/Shared/Description.purs`: stored under `tt-description-<game>` as
    `{ type, player, group, community }`, sent as the request, and made from a
    post by `viewOwnDescriptions` (`GET /api/games/:handle/own`, signed in), whose
    SQL steps 7 and 13 can reuse. The request fills in the viewer's timezone
    where the description has none; hours edited in the bar drop the one a
    post's description came with.
  - With nothing stored for the game, the description starts from the viewer's
    own post, player first, unstored until changed, so it follows the post
    until then. Clear all stores an empty one.
  - The bar's order is the prototype's rule, not usage: `on_card` game fields,
    the account's facts, then the other game fields, hours and microphone
    under More. Languages offer those the loaded posts use first, then every
    other. A filled chip is labelled with its field and value ("Rank:
    Platinum 1").
  - A card's tier is read from its marks: the ones that aren't `fit` are its
    misses, and a card with none goes last.
  - Back: the router tells `popstate` from a link (`ChangeRoute`'s `popped`),
    keeps each game's feed as a `FeedCache` (description, Showing, batches,
    cards opened, scroll position), and hands it only to a feed the browser
    went back or forward to; any other arrival starts afresh and replaces it.
    `history.scrollRestoration` is manual. The cache also carries the game,
    the viewer and the viewer's own posts, so a feed put back is drawn whole
    on its first render and scrolls back in its initializer; any other page
    Back reaches starts at the top. Cards are keyed by post id.
  - The router marks its root with the path it has drawn (`data-path`), and
    specs wait on that through `expectPage` rather than on the URL. The feed
    is `aria-busy` until its latest request answers. A navigation that Back or
    Forward overtakes before its tick is dropped.
  - The feed sets its own title and description once `viewGame` answers; an
    unknown handle is the not-found page with renderready's 404.
  - Load more has no spec, the seed having fewer than 21 posts in a game; it
    was run in the browser against `redesign_import` through the endpoint.

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
- Built in two parts. 7a: the endpoints, the Type and Game steps, the post
  screen with its preview, the existing-post check, Matches and `post.spec.ts`.
  7b: the signed-out flow, which is the register step inside the flow, Sign up
  with Discord beside the Discord input, and the choice between updating the
  existing post and discarding the draft.
- Settled here:
  - A player has one post per game and type, so all four endpoints are
    addressed by them at `/api/games/:handle/own/:type`: `viewOwnPost` (`GET`,
    the post if there is one, with its conversation count, and what the account
    holds), `createPost` (`POST`, `exists` if there already is one), `updatePost`
    (`PUT`, which moves `updated` to now) and `deletePost` (`DELETE`). The shapes
    are `Routes/Shared/Post.purs`: the post's content, answered by field key as a
    description is, and the account's facts and contacts, keyed by `game_contact`
    kind. Validation names each wrong field (`Server/Post/Infrastructure/ValidatePost.purs`),
    and a post keeps only what its type has.
  - The account's facts and contacts are written only where the screen gives
    them, so leaving one empty keeps what the account holds. The timezone is
    written with every post, since the hours are in it.
  - The screen asks every contact the game takes, not one game account, with
    the tracker hint on the kind the game's trackers read. Their labels are
    `Client/Shared/Contacts.purs`.
  - `viewMe`'s games carry the types posted, which mark "Your post" on the Game
    step and "You have one for …" on the Type step.
  - Paths: `/post?game=<handle>` is the Type step knowing the game, which the
    header's New post opens from a game's pages. `?from=edit` opens the post
    screen on the post, and `?from=feed` on the post (or the draft) with the
    feed's description over it; the feed's Edit and its Publish and Update post
    use them. The screen drops `?from` once it has the draft, which it keeps in
    `tt-draft-<game>-<type>`. Matches is `…/live`, with `?updated=1` after an
    edit.
  - Matches reads the post's description through `viewOwnDescriptions` and asks
    the feed with it. A player post's fits are counted by type, following the
    cursor while the batch still holds fits; a group's or community's is the
    feed's own count. Nothing notifies yet: step 13 puts the fit query's call in
    `createPost`.
  - The preview's card is `Draft.toCard`; a card with id 0, which only a draft
    has, names its post without linking to a page.
  - `post.spec.ts` posts in League of Legends, since `feed.spec.ts` asserts
    Valorant's feed whole.
  - The register step is `/signup` and `/signin`, the one sign-up screen,
    returning to the post screen with `?from=register`. Given that `back`
    (`Pages/Post/Register.purs`), they name the post and read Create account and
    publish, Sign in to publish and, at the nickname prompt, Publish post. Back
    on the post screen signed in, the draft publishes and the player lands on
    Matches.
  - A draft carries `signedOut`, set when it is written or published signed out.
    Whichever way the player then signs in (the register step, Sign up with
    Discord, the header's Sign in), the screen checks it against their post of
    the type: with one, it shows both cards under "You already have a …" with
    Update my post (the draft replaces the post whole, and `updated` moves) and
    Keep my post as it is (the draft is dropped and the feed opens). A create
    answered `exists`, another tab having published, shows the same choice.
  - Sign up with Discord returns to `/signin` like every Discord trip, and a
    player Discord doesn't know picks their nickname there, before the post
    screen, which registers them before they publish. The Discord
    contact then comes from the account: a blank contact in the draft no longer
    hides the account's.

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
- Settled here:
  - `viewPost` is `GET /api/games/:handle/posts/:id`, answering
    `{ post, blocked, owner }`: the card row with no marks, who blocked whom
    (`viewer` or `owner`) where either did, and for the owner only
    `{ expires, conversations, reveals }`. `expires` is the time the post runs
    out, so the 30 and 90 days stay in SQL. Not found is a missing id or one of
    another game, the gone page when `viewGame` knows the game. An id that isn't
    a number is the not-found page.
  - The card's columns but its marks are `Server/Post/Infrastructure/CardColumns.purs`,
    `Feed.sql`'s last select over a `post`, its `owner` and `parameters`, for
    step 9's `viewOwnPosts` too. `Feed.sql` keeps its own copy for `bench.sh`
    and `check.mjs`.
  - The card takes a `place`: `Listed`, `Preview` (the post screen's) or
    `Page { blocked, status }`. On a page the name is the `h1`, the type reads
    "Valorant player", Details goes, the contact button is filled, and until
    step 10 it is disabled. A block leaves no actions and no "You messaged".
    The owner's state, conversations and reveals are `Components/OwnPostStatus.purs`,
    which step 9 reuses; the conversation count is text until step 11 gives it
    the inbox, and it counts no unread until then.
  - Back shows when the history entry was opened from the game's feed. A link
    stamps each entry it opens with the path it left (`Script/Previous.purs`),
    so the entry knows after a reload and a trip back and forth; the router
    hands the post page what the entry says.
  - The way into the feed reads the description the feed would open with, what
    is stored or else the viewer's own posts. See what fits stores the post's
    description first, as Matches' See all does (`storeDescription`).
  - A player the owner blocked reads "There's no way to contact this post.",
    which doesn't say who blocked whom. Neither blocked line can be reached in
    the browser before step 12, whose `block.spec.ts` asserts the one it makes.
  - The title is "Night Owls · Valorant group | TeamTavern", the description the
    post's words cut to 155 characters.

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
- Settled here:
  - `viewOwnPosts` is `GET /api/own`, signed in: the games the player has posts
    in, in catalogue order, each with its posts in type order as
    `{ post, owner, description }`. `post` is the card row with no marks,
    `owner` the `OwnerView` the post page's owner reads too
    (`Routes/Shared/OwnPost.purs`: `expires`, `conversations`, `unread`,
    `reveals`, from `Server/Post/Infrastructure/OwnerColumns.purs`), and
    `description` the one the post makes, as `viewOwnDescriptions` gives it
    (`descriptionJson`), so See what fits stores it without a request of its own.
    A conversation is unread when the other side wrote after `owner_read_at`.
  - `renewPost` came forward from step 10, since the step's spec renews:
    `POST /api/games/:handle/posts/:id/renew`, signed in, not found for a post
    that isn't the player's. It moves `updated` to now and deletes the post's
    expiry notification (`ClearExpiry.purs`), which `updatePost` now does too,
    since an edit renews.
  - The home page's post is `ownCard` in `Card.purs`: heading, unmarked facts,
    the `OwnPostStatus`, then See what fits (outlined), Edit (text) and Renew,
    outlined while `renewDue` (the last week, or expired). The status says how
    many conversations are unread; it links nowhere until step 11.
  - Home is a `Slot__I Int` keyed by visit, so signing in or out onto `/` asks
    again. Signed out (`viewOwnPosts` refused) or without posts it is the start
    page. It fetches `viewGame` for each game posted in, for the facts' labels.
    After Renew it asks again and says "Renewed. Your post stays active for 30
    days from today." (90 for a community) in a toast; the keyed cards keep the
    focus on the button.
  - The titles are "TeamTavern: find players, groups and communities" signed out,
    which the shells now carry, with the lead line as the site's description,
    and "Your posts | TeamTavern" with posts.
  - The seed's `OwnerTester` (`owner@example.com`) has a post in each state: the
    Dota 2 group Kestrel's Nest, active; a Heroes of the Storm player post,
    expired; a Valheim player post in its last week. The games are ones no spec
    asserts the feed of, so the spec's renewal moves nothing another reads.

## Phase 3: contact

### 10. Contact panel and renewal

- Server: `revealContacts` by post id, signed in, counting the reveal on the
  post and returning the contacts and join links. `renewPost` is in from step
  9; the feed's and the post page's Renew call it here.
- Client, from `messaging.js`: the panel as a side panel and full-screen
  sheet, the preference ordering, the contact rows with Copy, Open the invite
  and Visit site, the "or message on TeamTavern" fold, the reply note before
  the first message; signed out, the button leads to sign up and back to the
  open panel. The message box is wired in step 11; here it is present and
  disabled with the note.
- Specs: `contact.spec.ts`: each preference gives its button label and order;
  signed out leads to sign up and back; a reveal shows on the owner's home
  page as Contacts shown once.
- Settled here:
  - `revealContacts` is `POST /api/games/:handle/posts/:id/contacts`, signed in,
    answering `{ contacts, discord_server, website }`: the owner's accounts of the
    game's contact kinds, Discord first, none for a community. Not found is a post
    of another game or one either side has blocked. One statement reads them and
    counts the reveal where it shows anything and the viewer isn't the owner, so
    every opening counts. The kind-to-column `case` is `ContactAccount.purs`, which
    `CardColumns` uses too.
  - The panel is `Components/ContactPanel.purs`: `useContactPanel`, which the feed,
    the post page and Matches use, and the `contactPanel` render function. It
    asks for the contacts as it opens, drawing the rest at once. Steam, a Discord
    server or invite and a website are links, `https://` added where the owner
    left it out. The message box is disabled until step 11; the ⋯ menu goes into
    the header's `tools` in step 12. The header is `Overlay.purs`'s `sidePanel`,
    a title with a line under it.
  - Signed out, the button goes to `/signup` returning to the page's path with
    `?contact=<id>`, which the page takes out of the address and opens the
    panel from through `viewPost`, so a post in a later batch opens too. The
    account pages keep their headings and say "You'll come straight back to
    X's post."; a Discord trip carries the same `back`.
  - Renew on the feed asks for the feed again, since the post moves; on the
    post page it reads the post again and puts `index, follow` back. Both toast
    as the home page does (`Shared/Renew.purs`).
  - The seed has every preference: `TeamFortress2Tester` is `offsite`, and
    `CommunityTester` (`community@example.com`) runs Payload Pals in Team
    Fortress 2, joined by `website`. `RenewTester` (`renew@example.com`) has
    expired posts in Rainbow Six Siege and Overwatch for the feed's and the
    page's Renew. `contact.spec.ts` opens no panel on Night Owls, whose owner's
    page `post-page.spec.ts` reads for no reveals.

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
- Built in two parts. 11a: the endpoints, the email, the panel's thread and
  message box, and the header's count. 11b: the inbox page, the home page's
  conversation link and `/design`'s messaging.
- Settled here:
  - Five routes answer with one `Conversation` shape (`Routes/Shared/Conversation.purs`):
    the post as an unmarked card row, the other side's nickname, for the owner
    the other player's post in the game (player, then group, then community),
    `readTo`, where the viewer had read to, and the messages. `viewInbox` is
    `GET /api/messages`; `viewConversation` `GET /api/messages/:id`;
    `viewPostConversation` `GET /api/games/:handle/posts/:id/messages`, the
    viewer's own conversation about a post, for the panel; `sendMessage` `POST`
    to that path, which starts the conversation; `sendReply` `POST /api/messages/:id`,
    from either side. Both views mark the viewer's side read, so reading is a
    `GET` with an effect, and only a player signed in can make it.
  - A message is kept as its lines, blank lines at either end and spaces at
    line ends dropped, at most 2000 characters, which the message box's
    `maxlength` also says. Nothing is sent for a blank one.
  - Sending locks the conversation, reads whether the recipient already had
    anything unread, adds the message and sets the sender's read mark, all in
    one transaction, so the sender's own message is never unread and two sent at
    once can't both email. Neither side of a block can send.
  - The message email (`SendMessageEmail.purs`) is plain, as the confirmation
    email is, until step 14 styles them all: who wrote, the message, the link to
    `/messages/<id>` and, to an owner whose post has expired, its renewal link.
    `Local` logs it.
  - `viewMe` counts the conversations with anything unread on either side
    (`Server/Conversation/Infrastructure/Unread.purs`). A page that reads a
    conversation or sends a message fires `tt-unread` on the window
    (`Script/Unread.purs`), and the header asks `viewMe` again, since its own
    asking on each visit can answer before the read lands.
  - The message box is `Components/Composer.purs`, a render function and
    `useComposer`, which the panel and the inbox share: Enter sends except on a
    phone, Shift+Enter and composing don't, the box grows to its `max-height`,
    and a message that fails stays in the box under an error. The thread is
    `Components/Thread.purs`. `useContactPanel` takes what to do once the viewer
    has written, so the feed, the post page and Matches mark their card.
  - `/messages` and `/messages/:id` are one `Pages/Messages.purs` in a slot
    keyed by nothing, so choosing a row keeps the list drawn; each visit asks
    for the inbox and, at the same time, the conversation, whose row it then
    marks read. An id that isn't a number is the not-found page; one the viewer
    isn't a side of shows "Choose a conversation." The page names itself
    "Ashen · Messages" once the conversation is in, and focuses the message box
    from 1024 px, where the list sits beside it.
  - About someone else's post, the owner's contacts fold under the header and
    are asked for as the fold first opens, which counts a reveal as the panel's
    opening does. The rows are `Components/InboxRow.purs`, the empty and
    signed-out states `Components/EmptyState.purs`, and `Shared/Renew.purs`
    renews any `{ id, type }`, for the inbox's Renew.
  - `OwnerView` carries `conversation`, the one its count opens: the latest
    with anything unread, else the latest. `ownPostStatus` makes the count a
    link to it on the home page and the owner's post page.
  - `messages.spec.ts` writes to HeroesOfTheStormTester, whom no other spec
    messages; its writers are new to each run. The owner's counts are read as
    they change, since earlier tests leave the owner conversations unread.

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
- Settled here:
  - A block is keyed by nickname: `block` is `POST /api/blocks/:nickname` and
    `unblock` `DELETE` to the same path, which the panel (the post's owner), the
    conversation (the other side), the toast's Undo and step 15's Unblock all
    name. Not found is a nickname nobody has or the viewer's own; blocking twice
    is blocking once. `viewBlocked` is `GET /api/blocks`, the viewer's own blocks
    by nickname, for step 15.
  - A report is `reportPost` (`POST /api/games/:handle/posts/:id/report`, against
    the owner) or `reportConversation` (`POST /api/messages/:id/report`, against
    the other side, about the conversation's post), with
    `{ reason, detail, block }` (`Routes/Shared/Report.purs`, which also holds the
    four reasons' labels for the form and the email). "Also block" is the same
    request and transaction. The detail may be as long as a message. The email
    goes to `ADMIN_EMAIL`, a required variable in both env files and in
    production's `.env`, after the transaction, and a failed send is logged, as
    the message email's is.
  - The two-way lookup is `blockedBetween` (`Server/Block/Infrastructure/Blocked.purs`),
    which `viewInbox`, both conversation views, `viewMe`'s unread count, the
    owner's counts (`OwnerColumns`), `revealContacts` and sending use.
    `Feed.sql` keeps its own copy. `viewOwnPost`'s conversation count doesn't
    apply it, since it counts what deleting the post deletes.
  - The menu, the confirmation and the report form are `Components/BlockReport.purs`:
    `useBlockReport`, which the contact panel and the inbox's conversation share,
    and `moreMenu` and `blockReportBody`, which stand the confirmation or the form
    in place of the body. The menu is a dropdown on a phone too, as the prototype
    has it. Escape leaves either for the body before it closes the panel.
    `useContactPanel` takes `{ onMessaged, onBlockChange, showToast }`: the feed
    asks for its first batch again, the post page reads the post again, and
    Matches, which now has a toast, loads again.
  - After a block the panel closes, or the inbox shows its list with nothing
    chosen, and the toast says "Kestrel is blocked." with Undo, which unblocks
    and says "Kestrel is unblocked." without opening anything again. A report
    that also blocks says "Report sent. Kestrel is blocked." with the same Undo,
    which keeps the report; the prototype said nothing of the report there. A
    toast's action dismisses it before it runs, so the action's own toast shows.
  - `/design` shows the panel with its menu open over the confirmation, and the
    report form asking for a reason, which `components.html` doesn't have.

## Phase 4: notifications and email

### 13. Fit notifications

- `Server/Feed/Fits.sql`: for one post, the other active posts in the game it
  fits, judged from each of those posts' owners' seats: their post as the
  description, this post as the one candidate, fit meaning no miss (brief 7.2,
  Decided). Derived from `Feed.sql`'s `described`, `answer` and `ranked`,
  with the description built from a post's rows rather than from JSON. Checked
  the way `check.mjs` checks the feed, against `notifyOwnersFitBy` in
  `game.js`.
- Server: on `createPost`, and on `renewPost` or the nonce renewal of an
  expired post, upsert a `fit` notification for every post it fits, blocked
  pairs excluded (`notification_fit_key`); `viewNotifications` grouped by own
  post in the settled order, hiding with `blockedBetween` what a block made
  after the notification was, as the prototype doesn't; `markNotificationsRead` for all and for one; the
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
- Settled here:
  - A notification and Matches judge from opposite seats. Matches is the posts
    that fit the new post (brief 6), from the new post's seat; a notification
    goes to the owner of each post the new post fits, from theirs. They agree
    only for a pair that fits both ways, which is the pair
    `notifications.spec.ts` publishes. As in the feed, a group or community is
    told of players alone.
  - `Server/Feed/Fits.sql` takes the post and now and returns the ids of the
    posts it fits. Every part names the part of `Feed.sql` it follows, and each
    seat's description is read from the post's rows as `descriptionJson` writes
    it. `notifyFits` (`Server/Post/Infrastructure/NotifyFits.purs`) wraps it in
    the upsert on `notification_fit_key`, so a renewal after expiry puts the
    same row back on top, unread. `createPost` calls it, and so do `renewPost`
    and `updatePost` when the post had expired, which their queries read
    before the update. An edit of an expired post renews it, and the prototype
    notified on it.
  - `redesign/feed/check-fits.mjs` checks `Fits.sql` against `Feed.sql` run from
    every active post's seat, with `descriptionJson` taken from the compiled
    `output/`. That is transitive through `check.mjs` to the prototype's
    `compare`, since `notifyOwnersFitBy` sees only the prototype's preset
    accounts. `--at` judges at an earlier time, when more of the dump was
    active; at 2025-09-01 the two agree on all 16,709 fitting pairs, and at
    2024-09-01 on all 388,620. The prototype's `compare` fits In-game leader as
    the feed does, two players when either can lead and a group that wants one
    with a player who can, and `check.mjs` agrees on every case, with the
    samples exported under the spelled-out handles. On
    `redesign_import` a Valorant post takes about 9 ms against today's 9 active
    posts, and about 155 ms judged at 2024-09-01, when 2,218 count as active.
  - The check found teams in the dump with ages of 100 and more for no limit.
    As a description such an age puts a birthday out of the date range, and
    both queries fail. The import now leaves an age outside 13 to 99, what
    `ValidatePost` allows, open.
  - The routes: `viewNotifications` (`GET /api/notifications`) gives the newest
    50, each with the player's own post, including its `expires`, and the
    fitting post for a `fit`. `readNotifications` (`POST /api/notifications/read`)
    and `readNotification` (`POST /api/notifications/:id/read`) mark them.
    `visibleNotification` (`Server/Notification/Infrastructure/Visible.purs`)
    hides a fit that a block came between, in the list and in `viewMe`'s count.
    Deleting either post deletes the row by cascade.
  - The list is `Components/Notifications.purs`, a render function the header
    fills. The header fetches the list each time the bell opens. Opening a row
    reads it, Mark all read reads them all, and the header then asks `viewMe`
    for the count again. An expiry row reads `termWords` from `OwnPostStatus`,
    in the home page's words, and has no time of its own.
  - `Client/Style/Components.scss` held only the notification section, which
    is now `Components/Notifications.scss`, so the monolith is gone.
  - The seed gives OwnerTester's Valheim post, in its last week, an unread
    expiry notification, as step 14's worker will.

### 14. Expiry, email and the worker

- Server worker, on a period read from the environment (an hour in
  production, seconds in `test.env`): posts entering their last week get an
  `expiry` notification (upsert); every owner with fit or expiry
  notifications created in the period that passed gets one email grouped by
  their posts (brief 8), each post with its new fits and, in its last week,
  Renew. Switches, `email_confirmed` and a missing address are honoured: the
  Matches switch governs the fits and the Renewals switch the expiries. A
  community's expiry asks whether its invite still works.
- `renewByNonce`: `GET /renew?nonce=` lands on the client, which calls the
  endpoint, then opens the game's feed with the description built from the
  renewed post under a note that it is active again; works signed out. Like
  `renewPost`, it clears the post's expiry and calls `notifyFits` when the
  post had expired.
- Emails as HTML with the site's palette, one module each: confirmation,
  password reset, the period's email, message, report. Every player email
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
  confirmation link confirms, once, and not after the email has changed; the
  password reset link sets a password that then signs in; a switch off sends
  nothing.
- Built in two parts. 14a: the mail stub, HTML for the emails already sent,
  `renewByNonce` and its landing, and `email.spec.ts` for those. 14b: the
  worker, its expiry rows and the period's email, with their specs.
- Settled here:
  - The worker sends each owner one email a period, as brief 8 decides, not a
    renewal email apart from a match email: fits and expiries are grouped by the
    owner's post in the same email.
  - Where an email links and whether it is sent is the `Mailer` in
    `Server/Infrastructure/Email.purs`: production's origin and SendGrid; the
    local stacks' empty origin, logged, unless `SENDGRID_API_URL` names something
    that takes SendGrid's requests. `Main.purs` sets that as the SendGrid
    client's base URL after the key, which resets it (`setBaseUrl` in bklaric's
    `JavaScript.Npm.Sendgrid`). Handlers that sent email by `Deployment` take the
    `Mailer`; the cookie still reads the `Deployment`.
  - An email is an `Email` of `Block`s (paragraph, quote, button, note), which
    make both its HTML and its text, escaped. The HTML is one table layout with
    the palette inline. Every player email links "Choose which emails you get"
    to `/account#emails`; the report to the admin doesn't. `sendEmail` logs a
    failed send and goes on; the password reset uses `deliver`, which fails the
    request, since the player waits for that email.
  - The message email names the expired post's place as listed under older
    posts, which is where it is, and offers Renew as a button.
  - The mail stub is `src/TeamTavern/MailStub/Main.purs`, built to
    `dist-test/mail-stub.js`, the test stack's `mail` service. It takes
    `POST /v3/mail/send` and shows an address's mail at `/mail?to=<address>`,
    newest first, each email in a frame whose links open the tab. Only
    `test.Caddyfile` routes `/mail` to it, on the site's origin, so the emails'
    relative links open the site.
  - `renewByNonce` is `POST /api/renew` with `{ nonce }`, signed out or as
    anyone, answering `{ handle, id, type, description }`. It shares
    `Server/Post/Infrastructure/Renew.purs` with `renewPost`, so both clear the
    expiry and notify on renewing an expired post. A link opened twice renews
    twice.
  - `/renew` (`Pages/Renew.purs`, `noindex`) stores the description and
    replaces itself with `/games/<handle>?renewed=<id>`. The feed takes the
    parameter out of the address, reads the post through `viewPost`, and puts
    "MailTester's player post is active again for 30 days. Showing what fits
    it." (a group or community by its name, "Your player post" to its owner) in
    the publish prompt's place, until the description changes.
  - The seed's `MailTester` (`mail@example.com`) has an active player post in
    Counter-Strike 2 and an expired one in Overwatch; `QuietTester`
    (`quiet@example.com`) has message emails off and a post in Counter-Strike 2.
  - A confirmation link that does nothing once the email has changed needs the
    account page to change it, so `account.spec.ts` in step 15 asserts it.
  - The worker is `Server/Worker.purs`, started by `Main.purs` beside the server
    on bklaric's `JavaScript.Node.Timers.setInterval`. The period is
    `WORKER_PERIOD` in seconds, an hour when unset, which production leaves it;
    `test.env` sets 2. Each run covers the time from when the one before it
    began, the first from the process's start, so the periods neither gap nor
    overlap while node runs. What is created while it is down, or committed in
    the moment a period closes, is on the bell but in no email.
  - An expiry row is inserted, not upserted: `AddExpiries.purs` gives one to
    each active post in its last week that has none, and leaves one it finds
    alone, since a refreshed row would be in every period's email. Renewal
    deletes it, so the post's next last week adds another.
  - `SendPeriodEmails.purs` takes the period's rows to a confirmed address, fits
    under the Matches switch (and not across a block, as the list hides them)
    and expiries under the Renewals switch, whether or not the owner has read
    them on the site. The subject counts them ("A new post fits yours", "2 of
    your posts expire soon", both joined by ", and"); the body greets the owner
    and heads each post "Your Team Fortress 2 community Night Shift", its fits
    listed as links to their pages ("FitsTester · player post") and its expiry
    as "It expires in 5 days.", rounded as the home page rounds, with Renew. A
    community joined by Discord or website is asked whether the link still
    works. `Email.purs` gained the `Heading` and `Links` blocks for it.
  - The seed's `FitsTester` (`fits@example.com`) has an expired Apex Legends post
    answering as ApexLegendsTester's in the other role, since two players in one
    slot don't fit, so renewing it puts a fit in the next
    period's email to `apex-legends@example.com`. `ExpiringTester`
    (`expiring@example.com`) has a Team Fortress 2 player post and the community
    Night Shift, both in their last week without their notice, which the first
    period gives them and emails together. QuietTester has renewal emails off
    and a Team Fortress 2 post in its last week. OwnerTester's seeded expiry is
    dated when its last week began, before the worker starts.

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
  inbox; a changed email shows unconfirmed, and the link sent to the old one
  confirms nothing; delete lands home signed out and
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
  and `/teams` to `/games/:handle`, the old handle turned into the new one as
  `legacy.game_map` in `redesign/import/mapping.sql` turns it (`lol` to
  `league-of-legends`, `csgo` to `counter-strike-2`); everything else old is a
  404. `/games` already redirects to the home page.
- Discord: `/signin` is the one redirect URI (step 4) and already registered;
  the manual check in `CLAUDE.md` is run against the dev stack once its
  database is the new model.

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
  rename it into place; `ADMIN_EMAIL` in the production `.env`; deploy the new
  bundles; start node; the Discord
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
- `/design` is kept or removed.
- The memory notes for this project are updated to say the relaunch shipped.

## What is not in the plan

Out of scope by the brief (16), and not to be started by accident: analytics,
competitions, community logos, rate limits, per-field weighting, near-miss
notifications, the nearest-region sort, a light theme, a "We found everyone"
action. The feed's prefill from the viewer's own post on a first visit (brief
7.1, Proposed; handover "Not yet prototyped") is the one Proposed item with no
prototype; it is built in step 6 as the brief describes it, and the brief's
status is updated with what it turned out to need.
