# TeamTavern

A team-finding site for gamers, live at <https://www.teamtavern.net>.

Players, groups and communities post what they are looking for in a game, by
rank, role, region, language and schedule, and message those who fit. The feed
puts the posts that fit yours first, and the site emails you when a new one
does. It covers Valorant, League of Legends, Counter-Strike 2, Dota 2, Apex
Legends, Overwatch, Rainbow Six Siege, Marvel Rivals, Team Fortress 2, Heroes
of the Storm and Valheim.

## One PureScript codebase

The browser app and the API server are both PureScript, compiled by a single
`spago build`, and they share their HTTP contract as types.

A route is a type. This one signs a player in:

```purescript
type StartSession =
    PostJson_ (Literal "sessions") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Internal_)

type BadContent = Variant
    ( unknownPlayer :: {}
    , wrongPassword :: {}
    , unknownDiscord :: { nickname :: String }
    )
```

The server's handler for it has to answer with one of those three responses,
and the client's call gets the same `Variant` back and matches on it:

```purescript
response <- fetchBody (Proxy :: _ StartSession)
    (inj (Proxy :: _ "password") { emailOrNickname, password })
response # onMatch
    { noContent: const $ navigateReplace_ "/"
    , badRequest: onMatch
        { wrongPassword: const $ showError "Wrong password." }
        (const $ showError "Something went wrong.")
    }
    (const $ showError "Something went wrong.")
```

Rename a field or drop a response, and whichever side still uses it stops
compiling.

| Part | Built with |
| --- | --- |
| Client | [Halogen](https://github.com/purescript-halogen/purescript-halogen) with Halogen Hooks, Sass, bundled by esbuild |
| Server | Node, with routing by Jarilo from [purescript-bklaric](https://github.com/bklaric/purescript-bklaric) |
| Database | PostgreSQL, queried with inline SQL |
| Serving | Caddy, which sends crawlers to a prerenderer and everyone else the single-page app |
| Email | Amazon SES |
| Tests | Playwright, driving the site through a browser against a seeded stack |
| Running it | Docker Compose |

## Layout

```
src/TeamTavern/
  Routes/         the HTTP contract, shared by client and server
  Server/         the Node API
  Client/         the Halogen app, its styles and static assets
  Shared/         static data both sides need
  Database/       SQL schema and seed data
stacks/           docker compose files, Caddyfiles and env files
test-playwright/  the Playwright suite
```

[CLAUDE.md](CLAUDE.md) describes the conventions in each of these in detail.

## Running it locally

You need:

- [Volta](https://volta.sh), which picks the Node and npm versions pinned in
  `package.json`. The compiler, spago and the rest of the toolchain are
  `devDependencies`.
- Docker.
- bash. On Windows that is Git Bash; the scripts do not run under `cmd.exe` or
  PowerShell.

Three packages are resolved from checkouts next to this one, so clone all four
side by side:

```bash
git clone https://github.com/bklaric/team-tavern
git clone https://github.com/bklaric/purescript-bklaric
git clone -b recursive-castable https://github.com/bklaric/purescript-untagged-union
git clone -b fix-variant-decoding https://github.com/bklaric/purescript-yoga-json
```

Then build and bring up the test stack, which seeds its own database:

```bash
cd team-tavern
npm install
./build.sh
docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml up -d
```

The site is at <http://localhost:8080>. The first `up` builds the prerenderer
image, which takes a while.

Every seeded game has a player to sign in as, such as `valorant@example.com`
with the password `tester-password`. The stack sends no real email: whatever
the site sends to an address shows at
`http://localhost:8080/mail?to=<address>`.

To throw the database away and start over:

```bash
docker compose --env-file stacks/test.env -f stacks/docker-compose.test.yml down -v
```

## Tests

```bash
./node_modules/.bin/playwright install chromium   # once
npm test                                          # reseeds the test stack, then runs every spec
npm test -- feed.spec.ts                          # only the specs you name
npm run typecheck                                 # typechecks the suite
```

`npm test` takes the test stack down and brings it back up itself, so it
starts from a fresh database each time.

## Licence

[AGPL-3.0](LICENSE).
