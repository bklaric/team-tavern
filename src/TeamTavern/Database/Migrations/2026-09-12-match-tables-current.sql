-- Brings an existing database up to TablesCurrent.sql. Run it once against
-- production, then delete this file.
--
-- It drops the competition table and player_profile.old_platform_id. Nothing
-- reads either, but the rows survive only in whatever a backup already holds,
-- so take one first if they are wanted. Against the compose stack:
--
--   docker exec postgres pg_dump -U bklaric -d team_tavern -a -t competition > competition.sql
--   docker exec postgres psql -U bklaric -d team_tavern -c "copy (select id, old_platform_id from player_profile where old_platform_id is not null) to '/backups/old_platform_id.csv' csv header"

begin;

-- Every field carries an icon and the seed always supplies one.
alter table field alter icon set not null;

-- Discord caps a tag at 37 characters. A no-op where a column already has it.
alter table player alter discord_tag type varchar(37);
alter table team alter discord_tag type varchar(37);

-- Superseded by the per-platform contact columns on player. Nothing has written
-- it since August 2021, and every player holding one also has a contact column
-- filled in.
alter table player_profile drop column old_platform_id;

-- One row, a Dota 2 league whose signup closed in 2021. No code reads the table.
drop table competition;

-- A field owns its options the way a game owns its fields, but only the second
-- of those constraints cascades, so deleting a game fails once the cascade
-- reaches its field rows.
alter table field_option drop constraint field_option_field_id_fkey;
alter table field_option add constraint field_option_field_id_fkey
    foreign key (field_id) references field(id) on delete cascade;

-- Close the gaps csgo and dota2 carry at the front of their field ordinals.
-- Only the order the ordinals put fields in is ever read, so this changes
-- nothing a page renders.
with renumbered as (
    select id, row_number() over (partition by game_id order by ordinal, id) as ordinal
    from field
)
update field
set ordinal = renumbered.ordinal
from renumbered
where field.id = renumbered.id and field.ordinal <> renumbered.ordinal;

commit;
