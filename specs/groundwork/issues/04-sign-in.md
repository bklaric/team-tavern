# Sign-in identity

Status: ready-for-agent

Exactly one identity per player, the email column as the contact email, Discord returning the address.

## To do

- `TablesCurrent.sql`: a check constraint `player_identity_check` on `player` requiring exactly one of `password_hash` and `discord_id` to be set; the column-level `unique` on `email` dropped; `player_lower_email_key` rebuilt as `unique index ... on player (lower(email)) where password_hash is not null`. Before applying, run the same predicates against the local restore of the 2026-09-12 dump and confirm zero violating rows. The `alter table` and `create index` statements go in this ticket's comments for the cutover to apply.
- `Session/Start/CheckPassword` and the password reset request match `lower(email) = lower($1)` only where `password_hash is not null`, so a Discord player sharing an address is never selected. Password reset is only offered to players with a password.
- The Discord authorize URLs in `Preboarding`, `Register` and `SignIn` request `identify email`. `FetchDiscordUser` reads `email` and `verified`; the Discord register and session-start handlers store the address when it is verified and the player's `email` is empty. An unknown Discord id registers a new player, never links to an existing one.
- `UpdatePlayerEmail` keeps working for every player and maps the unique violation to the same typed error; a Discord player editing to a password player's address is allowed.
- `CONTEXT.md` terms Sign-in identity and Contact email describe this; nothing in them should need changing.

## Done when

A new Discord sign-up lands with a contact email, an existing Discord player gets one on next sign-in, a Discord account and a password account sharing an address are two players and both can sign in, and the constraint and index exist in the test stack's schema.
