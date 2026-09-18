-- One game's posts as JSON, for the prototype feed. Run by export-sample.sh with
-- the variables handle and now. Player profiles become player posts, team
-- profiles of size party become group posts and those of size community become
-- community posts. Contact handles and emails stay out: only whether one exists.
--
-- Every post updated in the year before now is taken, and at least the 60 most
-- recently updated, so a game with no recent activity still has a feed.

with recursive continents (name, continent) as (
    select name, name from region where superregion_name is null
    union all
    select region.name, continents.continent
    from region join continents on region.superregion_name = continents.name
),

this_game as (
    select * from game where handle = :'handle'
),

player_posts as (
    select
        'p' || profile.id as id,
        'player' as type,
        player.nickname as name,
        player.nickname as owner,
        profile.updated,
        date_part('year', age(:'now'::timestamptz, player.birthday))::int as age,
        player.location,
        (select continent from continents where continents.name = player.location) as region,
        coalesce(player.languages, '{}') as languages,
        player.timezone,
        to_char(coalesce(player.weekday_from, player.weekend_from), 'HH24:MI') as online_from,
        to_char(coalesce(player.weekday_to, player.weekend_to), 'HH24:MI') as online_to,
        player.microphone,
        player.discord_tag is not null as has_discord,
        profile.new_or_returning,
        array_to_string(profile.about, E'\n') as about,
        array_to_string(profile.ambitions, E'\n') as ambitions,
        case when profile.platform is null then '{}' else array[profile.platform] end as platforms,
        (
            select json_object_agg(field.key, (
                select json_agg(field_option.key order by field_option.ordinal)
                from player_profile_field_value_option value_option
                join field_option on field_option.id = value_option.field_option_id
                where value_option.player_profile_field_value_id = field_value.id
            ))
            from player_profile_field_value field_value
            join field on field.id = field_value.field_id
            where field_value.player_profile_id = profile.id
        ) as fields,
        row_number() over (order by profile.updated desc) as recency
    from player_profile profile
    join player on player.id = profile.player_id
    where profile.game_id = (select id from this_game)
),

team_posts as (
    select
        't' || profile.id as id,
        case profile.size when 'community' then 'community' else 'group' end as type,
        team.name,
        owner.nickname as owner,
        team.id as team_id,
        profile.updated,
        team.age_from,
        team.age_to,
        coalesce((
            select array_agg(distinct continents.continent)
            from unnest(team.locations) location
            join continents on continents.name = location
        ), '{}') as regions,
        coalesce(team.languages, '{}') as languages,
        team.timezone,
        to_char(coalesce(team.weekday_from, team.weekend_from), 'HH24:MI') as online_from,
        to_char(coalesce(team.weekday_to, team.weekend_to), 'HH24:MI') as online_to,
        team.microphone,
        team.discord_tag is not null as has_discord,
        team.discord_server is not null as has_discord_server,
        team.website is not null as has_website,
        team.organization = 'organized' as organized,
        profile.new_or_returning,
        array_to_string(profile.about, E'\n') as about,
        array_to_string(profile.ambitions, E'\n') as ambitions,
        coalesce(profile.platforms, '{}') as platforms,
        (
            select json_object_agg(field.key, (
                select json_agg(field_option.key order by field_option.ordinal)
                from team_profile_field_value_option value_option
                join field_option on field_option.id = value_option.field_option_id
                where value_option.team_profile_field_value_id = field_value.id
            ))
            from team_profile_field_value field_value
            join field on field.id = field_value.field_id
            where field_value.team_profile_id = profile.id
        ) as fields,
        row_number() over (order by profile.updated desc) as recency
    from team_profile profile
    join team on team.id = profile.team_id
    join player owner on owner.id = team.owner_id
    where profile.game_id = (select id from this_game)
)

select json_build_object(
    'handle', (select handle from this_game),
    'title', (select title from this_game),
    'platforms', (select platforms from this_game),
    'now', :'now',
    'locations', (
        select json_agg(json_build_object('name', continents.name, 'continent', continents.continent) order by continents.name)
        from continents
        where not exists (select 1 from region child where child.superregion_name = continents.name)
    ),
    'fields', (
        select json_agg(json_build_object(
            'key', field.key,
            'label', field.label,
            'ilk', field.ilk,
            'options', (
                select json_agg(json_build_object('key', field_option.key, 'label', field_option.label) order by field_option.ordinal)
                from field_option where field_option.field_id = field.id
            )
        ) order by field.ordinal)
        from field where field.game_id = (select id from this_game)
    ),
    'players', (
        select coalesce(json_agg(to_jsonb(post) - 'recency' order by updated desc), '[]')
        from player_posts post
        where updated > :'now'::timestamptz - interval '1 year' or recency <= 60
    ),
    'teams', (
        select coalesce(json_agg(to_jsonb(post) - 'recency' order by updated desc), '[]')
        from team_posts post
        where updated > :'now'::timestamptz - interval '1 year' or recency <= 60
    )
);
