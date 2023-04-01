begin;

-- Store single select field values the same way as multi select field values.
insert into player_profile_field_value_option (player_profile_field_value_id, field_option_id)
select player_profile_field_value.id, player_profile_field_value.field_option_id
from player_profile_field_value
where player_profile_field_value.field_option_id is not null;

-- Drop unused columns.
alter table player_profile_field_value drop field_option_id;
alter table player_profile_field_value drop url;
alter table field drop domain;

-- Delete all url field values and fields.
delete from player_profile_field_value using field where field.id = field_id and field.ilk = 1;
delete from field where ilk = 1;

-- Use string constants instead of integers for field ilks.
alter table field alter ilk type text using
case
    when ilk = 2 then 'single'
    when ilk = 3 then 'multi'
end;

-- Create indexes for profile filtering.
create index on field(game_id);
create index on field_option(field_id);
create index on player_profile_field_value(player_profile_id);
create index on player_profile_field_value(field_id);
create index on player_profile_field_value_option(player_profile_field_value_id);
create index on player_profile_field_value_option(field_option_id);

commit;
