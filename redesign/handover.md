# Handover: redesign prototypes

Read `redesign/brief.md` first; it is the source of truth, and its
Decided/Proposed/Open statuses are kept current as things settle.

## Where things stand

- **Design system** (brief 14): dark by default, tavern palette, Inter, Lucide.
  `redesign/prototype/components.html` is the sheet showing every component and state.
- **Feed** (brief 4, 5, 7): `feed.html?game=valorant` runs on real posts from the
  12 September dump: the description bar, matching, tiers, the expired divider,
  Load more and the phone sheet.
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
  - Multi-option fields with few options are pills; languages are tokens with an
    "Add a language" select; group size is a count stepper.
  - The cover grid has no captions: four tiles across on a desktop, three on a
    phone, the title as each image's alt text. A game the player already has a
    post of this type in says "Your post" on its cover. No search yet (brief 14.3).
  - "Your post is live" carries the ember flame, not a green check: green means
    only a field that fits.
- **Platform** (brief 5, 7.2): `feed.html?game=apex` and `post.html?game=apex`.
  The options come from the seed's `game.platforms`, with every PC store (Steam,
  Origin, Riot, Battle.net, Ubisoft Connect) folded into PC; a game left with one
  platform has no field, so Valorant, Valheim and LoL show none. It takes several
  options on every post type, sits on every card after the languages, and is
  compared on every pair of types. A post that plays across platforms but names
  one, such as Apex's The Void, marked `≠ PC` for a PlayStation viewer, is today's
  data, not a gap in the model: a post picks every platform it plays on.
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

## Files

| File | What it is |
| --- | --- |
| `tokens.css`, `base.css`, `components.css` | The system: these become the new `Client/Style` |
| `prototype.css` | The prototype bar and stand-ins: not part of the system |
| `prototype.js` | `renderCard(post, marked)` and the shared helpers (icons, facts, slots) |
| `site.js` | What every page shares: the clock, games, the accounts the bar switches between, the conversation, block and report store that stands in for the server, the header, the prototype bar and toasts |
| `game.js` | One game's data and what the pages derive from it: game fields, posts (the dump's and the accounts'), made-up contacts, game extras (Riot ID, community kinds), `compare(post, type, description)`, `toCard`, and the field editors |
| `feed.js` | Feed only: the description, tiers, the bar and the phone sheet |
| `post.js` | Post creation, and the site's one sign-up screen |
| `messaging.js` | The contact panel, the conversation thread, inbox rows, block and report |
| `messages.js` | The inbox page |
| `conversations.js` | Handwritten conversations the store starts with; nobody in them is from the dump |
| `fixtures.js` | Handwritten posts for the sheet; production content stays out of the repo |
| `export-sample.sh` + `.sql` | Writes `data/<handle>.js` from the dev database (git-ignored). Valorant, Valheim, LoL and Apex are exported |
| `screenshot.mjs` | Phone and desktop screenshots; `--parts`, `--sections`, `--locale=en-US` |

## Using the pages

- The dashed bar above the header is the prototype's, not the design's. It picks
  who is viewing: signed out; Kestrel, who has a Valorant group post (so Valorant +
  group shows the existing-post check); or Vex, before a first post. Switching
  clears drafts. `?as=out|kestrel|vex` does the same from a URL.
- Discord is stood in for by a dialog: a new player (Mira) or Kestrel's account.
- Sign in with any email and password signs in as Kestrel.
- Drafts live in local storage per game and type (`tt-draft-<game>-<type>`).
- The bar is on every page. Conversations, blocks and reports outlive switching
  who is viewing, as a server's would, so Vex can message Night Owls and Kestrel
  then finds it unread in the inbox. **Start over** forgets them too.
- With a conversation open, **Reply as** the other side answers it. Dashed
  "Stand-in" toasts say what the site does offstage, such as whether a message
  sends an email (brief 10).
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

Notifications (11.3), the home page (11.2), the header's Games and account menus
(11.4), the account page (11.5), where blocked players are listed and unblocked,
and post pages (11.1), which the contact panel also opens from.

The post creation work is committed; contact and messaging and this handover
aren't. The game catalogue is stale too (Splitgate is dead, CS:GO becomes CS2)
and is to be handled separately.
