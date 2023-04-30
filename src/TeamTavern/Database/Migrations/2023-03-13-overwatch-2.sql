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

-- Game modes have also changed.

update field_option
set key = 'unranked', label = 'Unranked'
where id = 182;

update field_option
set key = 'competitive', label = 'Competitive'
where id = 184;

insert into field_option (field_id, key, label, ordinal)
values (31, 'custom-games', 'Custom games', 4);

update field_option
set ordinal =
    case
    when key = 'unranked' then 1
    when key = 'competitive' then 2
    when key = 'arcade' then 3
    when key = 'custom-games' then 4
    when key = 'leagues-tournaments' then 5
    end
where field_id = 31;

-- Alerts also need to be updated due to changed roles and game modes.

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\", \"leagues-tournaments\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\"]}"} '
where id = 449;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\", \"silver\", \"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\", \"leagues-tournaments\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\", \"silver\", \"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\", \"silver\", \"gold\"]}"} '
where id = 473;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 655;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 733;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 770;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"main-support\"]}"} '
where id = 1020;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\", \"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"arcade\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\", \"silver\"]}"} '
where id = 1070;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 1161;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 1465;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 1562;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 1563;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 1608;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"platinum\", \"diamond\"]}"} '
where id = 1799;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}"} '
where id = 1805;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\", \"silver\", \"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\", \"silver\", \"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\", \"silver\", \"gold\"]}"} '
where id = 1810;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\", \"diamond\", \"master\", \"grandmaster\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\", \"leagues-tournaments\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\", \"diamond\", \"master\", \"grandmaster\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"platinum\", \"diamond\", \"master\", \"grandmaster\"]}"} '
where id = 2041;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\", \"leagues-tournaments\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"flex-support\"]}"} '
where id = 2051;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"grandmaster\"]}"} '
where id = 2187;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\"]}"} '
where id = 2270;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\", \"main-support\", \"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"diamond\"]}"} '
where id = 2285;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"diamond\"]}"} '
where id = 2309;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"platinum\"]}"} '
where id = 2323;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\"]}"} '
where id = 2325;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}"} '
where id = 2330;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2333;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}"} '
where id = 2335;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}"} '
where id = 2336;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\"]}"} '
where id = 2342;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\"]}"} '
where id = 2346;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}"} '
where id = 2368;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}"} '
where id = 2409;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\", \"gold\", \"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\", \"platinum\"]}"} '
where id = 2375;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\", \"gold\", \"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\", \"platinum\"]}"} '
where id = 2376;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\", \"leagues-tournaments\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2377;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"projectile-dps\"]}"} '
where id = 2379;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\"]}"} '
where id = 2389;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"master\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\"]}"} '
where id = 2401;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\", \"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\", \"platinum\", \"diamond\"]}"} '
where id = 2408;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"platinum\"]}"} '
where id = 2413;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"platinum\"]}"} '
where id = 2417;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\"]}"} '
where id = 2418;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 2427;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\"]}"} '
where id = 2429;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"flex-support\"]}"} '
where id = 2430;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 2436;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\"]}"} '
where id = 2437;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2438;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\", \"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2446;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\", \"gold\"]}"} '
where id = 2473;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}"} '
where id = 2485;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\", \"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2486;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\"]}"} '
where id = 2491;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"hitscan-dps\", \"projectile-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"platinum\"]}"} '
where id = 2492;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2526;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"bronze\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"bronze\"]}"} '
where id = 2540;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2546;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"platinum\", \"diamond\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}"} '
where id = 2550;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2562;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2563;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"diamond\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"diamond\"]}"} '
where id = 2568;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"bronze\", \"silver\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"leagues-tournaments\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\"]}"} '
where id = 2570;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\", \"leagues-tournaments\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\", \"main-support\", \"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 2575;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 2589;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}"} '
where id = 2590;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\", \"gold\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"unranked\", \"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"projectile-dps\", \"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\", \"platinum\"]}"} '
where id = 2600;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"diamond\"]}"} '
where id = 2603;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"main-support\", \"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"silver\"]}"} '
where id = 2613;

update alert
set fields = '{"{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"main-support\"]}"} '
where id = 2628;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\", \"master\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\"]}"} '
where id = 2650;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\", \"master\"]}","{\"fieldKey\": \"interest\", \"optionKeys\": [\"competitive\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\"]}"} '
where id = 2651;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"silver\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"tank\", \"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}","{\"fieldKey\": \"tank-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 2425;

update alert
set fields = '{"{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"main-support\", \"flex-support\"]}","{\"fieldKey\": \"support-rank\", \"optionKeys\": [\"gold\"]}"} '
where id = 2646;

update alert
set fields = '{"{\"fieldKey\": \"damage-rank\", \"optionKeys\": [\"diamond\", \"master\"]}","{\"fieldKey\": \"role\", \"optionKeys\": [\"hitscan-dps\", \"projectile-dps\"]}"} '
where id = 2649;

commit;
