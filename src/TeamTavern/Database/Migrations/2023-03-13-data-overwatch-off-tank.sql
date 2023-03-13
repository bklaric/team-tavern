-- Since Overwatch 2 came out, teams now have 5 players instead of 6 and 1 tank instead of 2.

-- Values based on production database:
-- Overwatch game id = 8
-- Role field id = 30
-- Main tank option id = 176
-- Off tank option id = 177

-- Has only main tank -> do nothing
-- Has only off tank -> change it to main tank
-- Has both -> delete off tank

begin;

-- Number of profiles with either main tank, off tank, or both.
-- Players.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from player_profile as profile
    join player_profile_field_value as value on value.player_profile_id = profile.id
    join player_profile_field_value_option as option on option.player_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
select count(*)
from profile_tank_values;

-- Teams.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from team_profile as profile
    join team_profile_field_value as value on value.team_profile_id = profile.id
    join team_profile_field_value_option as option on option.team_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
select count(*)
from profile_tank_values;


-- Players.
-- Update off tank only to main tank.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from player_profile as profile
    join player_profile_field_value as value on value.player_profile_id = profile.id
    join player_profile_field_value_option as option on option.player_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
update player_profile_field_value_option option
set field_option_id = 176
where option.field_option_id = 177 and not exists (select * from profile_tank_values where value_id = option.player_profile_field_value_id and 176::int = any (profile_tank_values.tank_values));

-- Delete off tank if main tank exists.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from player_profile as profile
    join player_profile_field_value as value on value.player_profile_id = profile.id
    join player_profile_field_value_option as option on option.player_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
delete from player_profile_field_value_option option
where option.field_option_id = 177 and exists (select * from profile_tank_values where value_id = option.player_profile_field_value_id and 176::int = any (profile_tank_values.tank_values));

-- Teams.
-- Update off tank only to main tank.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from team_profile as profile
    join team_profile_field_value as value on value.team_profile_id = profile.id
    join team_profile_field_value_option as option on option.team_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
update team_profile_field_value_option option
set field_option_id = 176
where option.field_option_id = 177 and not exists (select * from profile_tank_values where value_id = option.team_profile_field_value_id and 176::int = any (profile_tank_values.tank_values));

-- Delete off tank if main tank exists.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from team_profile as profile
    join team_profile_field_value as value on value.team_profile_id = profile.id
    join team_profile_field_value_option as option on option.team_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
delete from team_profile_field_value_option option
where option.field_option_id = 177 and exists (select * from profile_tank_values where value_id = option.team_profile_field_value_id and 176::int = any (profile_tank_values.tank_values));


-- Delete off tank option.
delete from field_option
where id = 177;

-- Rename main tank to just tank.
update field_option
set label = 'Tank', key = 'tank'
where id = 176;

-- Fix ordinals for roles after offtank.
update field_option
set ordinal = ordinal - 1
where id = 178 or id = 179 or id = 180 or id = 181;


-- Sanity check number of profiles with either main tank, off tank, or both.
-- Players.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from player_profile as profile
    join player_profile_field_value as value on value.player_profile_id = profile.id
    join player_profile_field_value_option as option on option.player_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
select count(*)
from profile_tank_values;

-- Teams.
with profile_tank_values as (
  select profile.id profile_id, value.id value_id, array_agg(option.field_option_id) tank_values
  from team_profile as profile
    join team_profile_field_value as value on value.team_profile_id = profile.id
    join team_profile_field_value_option as option on option.team_profile_field_value_id = value.id
  where profile.game_id = 8
    and value.field_id = 30
    and (option.field_option_id = 176 or option.field_option_id = 177)
  group by profile.id, value.id
)
select count(*)
from profile_tank_values;

commit;
