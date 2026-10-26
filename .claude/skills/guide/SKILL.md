---
name: guide
description: Research, write and ship a TeamTavern guide, a Markdown page served at /guides/<slug>. Use whenever a guide is proposed, briefed, written, refreshed or rechecked against a game's new season or patch, and whenever /blog-brief or /blog-write is used for TeamTavern — even if the user only says "write the Overwatch guide" or "Deadlock's season ended, check its guide".
---

# Writing a TeamTavern guide

A guide answers a question about playing with other people that a game's feed
can't: who can queue ranked together, what a tournament asks of a team, how
to find a trio for it. It ends by sending the reader to the feed to find those
people. It is never gameplay advice or esports news, and never "<game> team
finder" or "<game> LFG", which is what the feed itself is for.

Guides are evergreen pages at fixed URLs with an "Updated" date, not a dated
blog. A game guide is a spoke of the guide to joining an esports team, which
names every game's team format and links to each game's guide.

## Where things are

- The guides are `src/TeamTavern/Server/Guide/<slug>.md`. CLAUDE.md, under
  "Server conventions", says how they are served and how a time of day is
  written.
- Each guide is imported in `Guides.js` beside them and has an entry in
  `Guides.purs` (slug, heading, title, description, published, updated, games,
  html).
- `Client/Pages/Guide.purs` puts ad spots after the In short box and two
  sections later, so every guide keeps the shape below.
  `Client/Pages/Document.scss` styles `.guide-cover`, `.guide-chart` and
  `.guide-note`.
- A game's seed, `src/TeamTavern/Database/Seed/Games/<Game>.sql`, holds the
  fields, options and contacts a post in that game has. A guide's advice on
  posting names them exactly.
- Briefs are `briefs/<slug>-brief.md`.
- The private working notes are `GUIDES.md` in the planning folder outside
  the repo; Claude's memory has its path. It holds the Search Console
  evidence, topics not yet written, each guide's unconfirmed facts and
  recheck dates, caveats on the post data, and research methods that worked.
  Search Console figures stay there, never in the repo, which is public.
- The pattern to follow is the sibling guides. For a tournament guide, read
  `marvel-rivals-championship-team.md` or `fncs-team.md`. For a ranked guide,
  read `the-finals-ranked-with-friends.md` or `valorant-ranked-with-friends.md`.
  Read one or two before writing.

## Using /blog-brief and /blog-write

They work well for research, briefs and a first draft. Their defaults don't
fit these guides, so these override them:

- **No** "Key Takeaways" box, internal-link placeholder zones, `[PERSONAL
  EXPERIENCE]` markers, frontmatter, JSX, SVG charts, stock images or image
  credits, and no word count target. A guide is as long as its facts.
- **The shape below** replaces their templates.
- **Sources** follow the rules below, not their citation style.
- **Their scoring is advice.** The owner's standing choices (no byline or
  Person, the organisation as author, the cover as the only image) are not
  defects, and neither is anything listed as a house rule here.

## Workflow

1. **Topic.** Check GUIDES.md for evidence and topics already weighed. A good
   topic is a question with a rules-shaped answer, such as party size, what
   ranks can play together or what a tournament requires. Those rank within
   days. Check Google autocomplete and what the top results and AI Overview
   say, since being right where they are wrong is the guide's edge.
2. **Research.** Primary sources first: the publisher's rules, patch notes,
   help pages and rules PDFs. Note each fact's source and date, and keep a
   list of what stays unconfirmed. Publishers' pages often refuse WebFetch;
   GUIDES.md lists the ways round that. Counts from the site's own posts come
   from a production backup the user provides, restored into a scratch
   database.
3. **Brief.** Write `briefs/<slug>-brief.md`: the target query and reader,
   the facts with sources, what's unconfirmed, the outline, the slug, and
   additions to the join and make guides. Show it to the user before writing
   the guide.
4. **Write** the guide in the shape below.
5. **Register** it: an import and an export in `Guides.js`, and a `foreign
   import` and an entry in `Guides.purs`. The `games` field names the game's
   handle and title, which puts its cover in the list and on shared links.
6. **Spokes.** For a game guide, add the game to the join guide: a row in the
   table under "Make your account eligible", an H3 under "Your game's team
   format" with the cover image, a link whose text is the guide's H1, and the
   feed link, and its team size in the opening definition. Add a row to the
   make guide's table under "Set up the team in your game's format", and its
   team size to that guide's FAQ. Move both guides' `updated`.
7. **Specs.** Add a test to `test-playwright/integration/guides.spec.ts` that
   goes from the join guide to the new guide by its H1, checks its table, and
   follows its feed link. Add the slug to the guide lists in
   `crawlers.spec.ts`, `accessibility.spec.ts`, `phone.spec.ts` and
   `test-playwright/screenshots.mjs`.
8. **Verify.**
   - Run `.\bundle.ps1 -environment local` and `npm run typecheck`.
   - Check that no other session's `playwright test` is running, since the
     setup restarts the one local stack.
   - Run `npm test -- guides.spec.ts crawlers.spec.ts accessibility.spec.ts phone.spec.ts ads.spec.ts --timeout=120000`.
     The longer timeout keeps accessibility checks passing on a busy machine.
   - Look at the page with `"$(volta which node)" test-playwright/screenshots.mjs --only=<slug>`.
9. **Review.** Run a blog-reviewer agent on the file. List the standing
   choices above as non-defects, and point it at the sibling guides as the
   standard. Check each finding against the sources before applying it,
   since reviews have been wrong about what a page says. Rerun the specs
   after the fixes.
10. **Date and commit.** `published` and `updated` are the user's launch day,
    a placeholder until they name it. `updated` changes when a fact changes,
    not for a typo. The user approves every commit and every deploy.
11. **Record.** In GUIDES.md, note what the guide rests on, what's
    unconfirmed, and when to recheck it (the next season, a patch, an
    announcement).

A refresh follows the same steps from research onward. Move `updated`, and
fix every sentence in the join and make guides that repeats a changed fact.

## The shape

- **Opening paragraph.** The fact the reader needs that other pages miss.
  Lead with the answer.
- **Second paragraph:** "This guide is for …" (the reader and their
  situation), "It covers …" (the sections in order), then "For other games,
  see [every game's team format](/guides/join-an-esports-team#your-games-team-format)."
- **In short:** a blockquote opening `> **In short**`, then four to six
  bullets. Each one stands on its own, because these get quoted alone.
- **The first H2** opens with the cover:
  `<img class="guide-cover" src="/images/games/400/<handle>.webp" alt="" width="400" height="600" loading="lazy">`
  then the section's text on the same line.
- **The body sections.** The rules, how they play out, then recruiting.
- **The recruiting section** ends with a "Post on TeamTavern" paragraph that
  names the seed's real field labels and options ("tick "FNCS" under
  "Looking for"") and the contact to add, then the feed link on a line of
  its own: `[<Game> players and groups on TeamTavern](/games/<handle>)`.
- **Frequently asked questions:** an H2 with H3 questions phrased the way
  people search them, and answers that stand on their own.
- **The closing H2,** "Find your <partner/trio/team>": three or four bullets,
  then the call to post with the feed link, and the official Discord if the
  game has one.
- **H1 and title.** The H1 (`heading`) is short and plain. The `title` is
  phrased like the search query, and the `description` is one or two
  sentences that say what the reader gets.
- **The slug** is the URL for good. Choose one that outlives a season, a year
  or a format change: `fncs-team`, not `fncs-2027-trios`.

## House rules

- **Sources.** Link the words that name the source, once per source per
  section, never "(Riot)" in parentheses. Prefer primary sources to
  Wikipedia. A third party is the primary source for its own data (HLTV,
  Fortnite Tracker, deadlock-api.com). Quote a rule word for word only where
  its wording matters.
- **Never name or link Reddit or community Discord servers.** A game's
  official Discord is fine.
- **Hedge in place.** A fact from an older rule set, a player report or a
  guess says so in its own sentence ("Under 2026's rules", "players
  report"). This matters most in the In short box and the FAQ, which are
  read alone. Leave out what can't be sourced rather than state it.
- **At most one table per guide,** since the specs' `getByRole("table")` is
  strict. Bold the first column.
- **Charts** are HTML bars, never SVG, so text keeps its size on a phone:
  a `figure.guide-chart` as in the join guide's role chart. List a game's
  roles in the game's own order.
- **Times of day** are UTC in `<time>HH:MM</time>`, with
  `<span data-local="your time">UTC</span>` where the zone is named and a
  `<p class="guide-note" data-local="…{zone}…">` note under a schedule. The
  Rocket League guide shows all three.
- **Links within the site** are paths (`/games/<handle>`,
  `/guides/<slug>#<heading-id>`), and the router handles them. Heading ids are
  GitHub-style: lowercase, punctuation dropped, spaces as hyphens. Specs and
  links follow a changed heading; never keep old text for their sake.
- **Voice.** Plain, specific sentences with contractions and British spelling
  (practise as a verb, organiser). No marketing, no filler, no "in today's
  competitive landscape".
- **A paid link is never unlabelled.** It carries `rel="sponsored"`, and the
  user decides how it's labelled. Cite a sponsor only where it's the best
  source on merit.
