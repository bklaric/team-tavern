# Sign-in identity

Status: resolved

Exactly one identity per player, the email column as the contact email, Discord returning the address.

## To do

- `TablesCurrent.sql`: a check constraint `player_identity_check` on `player` requiring exactly one of `password_hash` and `discord_id` to be set; the column-level `unique` on `email` dropped; `player_lower_email_key` rebuilt as `unique index ... on player (lower(email)) where password_hash is not null`. Before applying, run the same predicates against the local restore of the 2026-09-12 dump and confirm zero violating rows. The `alter table` and `create index` statements go in this ticket's comments for the cutover to apply.
- `Session/Start/CheckPassword` and the password reset request match `lower(email) = lower($1)` only where `password_hash is not null`, so a Discord player sharing an address is never selected. Password reset is only offered to players with a password.
- The Discord authorize URLs in `Preboarding`, `Register` and `SignIn` request `identify email`. `FetchDiscordUser` reads `email` and `verified`; the Discord register and session-start handlers store the address when it is verified and the player's `email` is empty. An unknown Discord id registers a new player, never links to an existing one.
- `UpdatePlayerEmail` keeps working for every player and maps the unique violation to the same typed error; a Discord player editing to a password player's address is allowed.
- `CONTEXT.md` terms Sign-in identity and Contact email describe this; nothing in them should need changing.

## Done when

A new Discord sign-up lands with a contact email, an existing Discord player gets one on next sign-in, a Discord account and a password account sharing an address are two players and both can sign in, and the constraint and index exist in the test stack's schema.

## Comments

Landed on branch `sign-in-identity`.

- **The statements for the cutover** are `src/TeamTavern/Database/Migrations/2026-09-13-sign-in-identity.sql`, one transaction over `TablesBase.sql`, which is the schema before this change. The two predicates in its header returned 0 against the development database holding the 2026-09-12 restore (31196 players, 3870 with only a Discord id), and the migration is applied there. `TablesBase.sql` with the migration and `TablesCurrent.sql` alone give identical `pg_dump --schema-only` output. `CLAUDE.md` describes this migration workflow.
- **Discord players change the contact email without a password**, having none; password players still confirm with theirs. The client offers Change email to every player on their own page.
- **The verified Discord address is stored only if it passes the same validation as a typed address**, so an empty or overlong one leaves the email empty rather than failing sign-in.
- **`DISCORD_API_URL`** is a new optional variable, defaulting to `https://discord.com/api`; production leaves it unset. The test stack points it at a PureScript stand-in for Discord's user endpoint, and `test-playwright/integration/sign-in.spec.ts` covers:
  - Discord sign-up with a verified, unverified and unusable address;
  - the fill on a later sign-in, which never overwrites an existing email;
  - a shared address between a Discord and a password player, both signing in;
  - password sign-in and reset not finding Discord players;
  - both kinds of email change;
  - the constraint and partial index in the test schema.
- **Verified by** `spago build`, `npm run typecheck` and `npm test` (15 passed), and the development site still serving listings and password sign-in on the migrated database.
- **One edge left for the cutover:** a password reset link a Discord player requested in the hour before the migration would fail with a 500 if used, because the constraint rejects giving that player a hash.
