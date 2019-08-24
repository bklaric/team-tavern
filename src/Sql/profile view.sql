-- Select after filter.
select
    profile.id,
    game.handle,
    player.nickname,
    -- profile.summary,


    -- field.key,
    -- field_value.url,
    -- single.key,
    -- multi.key

    json_agg(json_build_object(
        'key', field.key,
        case
            when field.type = 1 then 'url'
            when field.type = 2 then 'option'
            when field.type = 3 then 'options'
        end,
        case
            when field.type = 1 then field_value.url
            when field.type = 2 then single.key
            when field.type = 3 then multi.key
        end
    ))

    -- ,
    -- coalesce(
    --     json_agg(json_build_object(
    --         'id', field_value.id,
    --         'fieldId', field_value.field_id,
    --         'data', field_value.data
    --     ))
    --     filter (where field_value.id is not null),
    --     '[]'
    -- ) as "fieldValues"
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
left join field_value on field_value.profile_id = profile.id
join field on field.id = field_value.field_id
left join field_value_option on field_value_option.field_value_id = field_value.id
left join field_option as single on single.id = field_value.field_option_id
left join field_option as multi on multi.id = field_value_option.field_option_id
    -- or field_option.id = field_value_option.field_option_id
group by profile.id, game.handle, player.nickname, profile.summary, profile.created
order by profile.created desc;





-- Select after filter.
select
    profile.id,
    game.handle,
    player.nickname,
    field.key,
    field.type,
    to_json(field_value.url) as url,
    to_json(single.key) as single,
    json_agg(multi.key) as multi
from profile
join player on player.id = profile.player_id
join game on game.id = profile.game_id
left join field_value on field_value.profile_id = profile.id
join field on field.id = field_value.field_id
left join field_value_option on field_value_option.field_value_id = field_value.id
left join field_option as single on single.id = field_value.field_option_id
left join field_option as multi on multi.id = field_value_option.field_option_id
group by profile.id, game.handle, player.nickname, profile.summary, profile.created, field.key, field_value.url, single.key
order by profile.created desc;






select
    id,
    handle,
    nickname,
    summary,
    coalesce(
        json_agg(json_build_object(
            'key', key,
            case
                when type = 1 then 'url'
                when type = 2 then 'option'
                when type = 3 then 'options'
            end,
            case
                when type = 1 then url
                when type = 2 then single
                when type = 3 then multi
            end
        )) filter (where field_value_id is not null),
        '[]'
    ) as "values"
from (
    select
        profile.id,
        game.handle,
        player.nickname,
        profile.summary,
        field.key,
        field.type,
        field_value.id as field_value_id,
        to_json(field_value.url) as url,
        to_json(single.key) as single,
        json_agg(multi.key) as multi
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    left join field_value on field_value.profile_id = profile.id
    join field on field.id = field_value.field_id
    left join field_value_option on field_value_option.field_value_id = field_value.id
    left join field_option as single on single.id = field_value.field_option_id
    left join field_option as multi on multi.id = field_value_option.field_option_id
    group by
        profile.id,
        game.handle,
        player.nickname,
        profile.summary,
        profile.created,
        field.key,
        field.type,
        field_value.id,
        field_value.url,
        single.key
    order by profile.created desc
) as haha
group by id, handle, nickname, summary;

-- select id
-- from crosstab(
--     '
--     select
--         profile.id,
--         field.key as field,
--         array_agg(field_option.key) as options
--     from profile
--     join field_value on field_value.profile_id = profile.id
--     left join field_value_option on field_value_option.field_value_id = field_value.id
--     join field on field.id = field_value.field_id
--     join field_option on field_option.id = field_value.field_option_id
--         or field_option.id = field_value_option.field_option_id
--     group by profile.id, field.key
--     order by profile.created;
--     ',
--     'select key from field where type = 2 or type = 3 order by id;'
-- ) as (id int, rank text[], position text[])
-- where ('hard-support' = any(position) or 'off-lane' = any(position))
--     and ('archon' = any(rank));
