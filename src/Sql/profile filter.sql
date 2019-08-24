select
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    field_option.key as option
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
order by profile.created;

select
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    field_option.key as option
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
order by profile.created;

select
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    array_agg(field_option.key) as options
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
group by profile.id, game.handle, player.nickname, profile.summary, field.key
order by profile.created;

select
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    array_agg(field_option.key) as options
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
group by profile.id, game.handle, player.nickname, profile.summary, field.key
order by profile.created;

select *
from crosstab('
select
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    array_agg(field_option.key) as options
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
group by profile.id, game.handle, player.nickname, profile.summary, field.key
order by profile.created;
', 'select key from field where type = 2 or type = 3 order by id;') as
(id int, handle text, nickname text, summary text, rank text[], position text[])
where ('hard-support' = any(position) or 'off-lane' = any(position)) and ('archon' = any(rank));

-- *THE* FILTERING QUERY
-- Filter profiles in first and fields in second query by game id.
select id
from crosstab(
    '
    select
        profile.id,
        field.key as field,
        array_agg(field_option.key) as options
    from profile
    join field_value on field_value.profile_id = profile.id
    left join field_value_option on field_value_option.field_value_id = field_value.id
    join field on field.id = field_value.field_id
    join field_option on field_option.id = field_value.field_option_id
        or field_option.id = field_value_option.field_option_id
    group by profile.id, field.key
    order by profile.created;
    ',
    'select key from field where type = 2 or type = 3 order by id;'
) as (id int, rank text[], position text[])
where ('hard-support' = any(position) or 'off-lane' = any(position))
    and ('archon' = any(rank));

select *
from crosstab('
select
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    array_agg(field_option.key) as options
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
group by profile.id, game.handle, player.nickname, profile.summary, field.key
order by profile.created;
', 'select key from field where type = 2 or type = 3 order by id;') as
(id int, handle text, nickname text, summary text, rank text[], position text[])
where 'hard-support' = any(position) or 'off-lane' = any(position);

select *
from crosstab('
select
    dense_rank() over (order by profile.id, field_option.key),
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    field_option.key as option
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
order by field_option.key;
', 'select key from field where type = 2 or type = 3 order by id;') as
(dense int, id int, handle text, nickname text, summary text, rank text, position text);

select
    hah.id,
    hah.handle,
    hah.nickname,
    hah.summary,
    case when hah.rank is not null then hah.rank else heh.rank end as rank,
    case when hah.position is not null then hah.position else heh.position end as position
from crosstab('
select
    dense_rank() over (order by profile.id, field_option.key),
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    field_option.key as option
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
order by field_option.key;
', 'select key from field where type = 2 or type = 3 order by id;') as  hah
 (dense int, id int, handle text, nickname text, summary text, rank text, position text),
 crosstab('
select
    dense_rank() over (order by profile.id, field_option.key),
    profile.id,
    game.handle,
    player.nickname,
    profile.summary,
    field.key as field,
    field_option.key as option
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
join field_value on field_value.profile_id = profile.id
left join field_value_option on field_value_option.field_value_id = field_value.id
join field on field.id = field_value.field_id
join field_option on field_option.id = field_value.field_option_id
    or field_option.id = field_value_option.field_option_id
order by field_option.key;
', 'select key from field where type = 2 or type = 3 order by id;') as  heh
 (dense int, id int, handle text, nickname text, summary text, rank text, position text)
 where hah.dense != heh.dense and hah.dense = 1;
