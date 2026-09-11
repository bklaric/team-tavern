# Schema and migration

Status: ready-for-agent

The new tables and the one-transaction migration from the current schema, developed and rehearsed against the restored production dump in the local database.

## To do

- A dated migration in `src/TeamTavern/Database/Migrations/` that creates: `sign_in_identity` (one per player, kind plus provider id or password hash, unique per kind), `contact_email` columns on `player`, `game` with `igdb_id` and cover path, `game_field` limited to kinds rank, role and mode with at most one per kind per game, `post` with direction, platform, availability, microphone, new or returning, about, group fields and wanted fields, `post_field_value` with a single or set value by direction, and a unique index on player, game and direction.
- In the same transaction: move email and password and Discord id into identities; join about and ambitions; map player profiles and team profiles to posts with `renewed` set from `updated`; copy team name and Discord server onto the post; drop secondary rank fields and their values; drop `alert`, `team`, `team_profile*`, `player_profile*`.
- Update `TablesCurrent.sql` to the resulting schema and `Seed/Games/` to the reduced field set.
- A rehearsal script that restores a dump into a scratch database and applies the migration, with row-count assertions: posts equal profiles, no player lost, every post has a field value only for kept fields.

## Done when

The rehearsal passes against the 2026-09-11 dump and `TablesCurrent.sql` matches what the migration produces.
