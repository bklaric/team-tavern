-- One game's posts as JSON, for the prototype feed. Run by export-sample.sh with
-- the variables handle and now, against the dump imported into the new schema
-- (redesign/import). Contact handles and emails stay out: only whether one
-- exists.
--
-- Every post updated in the year before now is taken, and at least the 60 most
-- recently updated players and the 60 most recently updated groups and
-- communities, so a game with no recent activity still has a feed.

with this_game as (
    select * from game where handle = :'handle'
),

posts as (
    select
        post.id::text as id,
        post.ilk as type,
        case when post.ilk = 'player' then owner.nickname else post.name end as name,
        owner.nickname as owner,
        post.updated,
        case when post.ilk = 'player'
            then date_part('year', age(:'now'::timestamptz, owner.birthday))::int end as age,
        case when post.ilk = 'player' then owner.country end as country,
        case when post.ilk = 'player' then owner.languages else post.languages end as languages,
        post.regions,
        post.age_from,
        post.age_to,
        owner.timezone,
        to_char(post.online_from, 'HH24:MI') as online_from,
        to_char(post.online_to, 'HH24:MI') as online_to,
        post.microphone,
        post.contact_preference as reach,
        owner.discord_tag is not null as has_discord,
        post.discord_server is not null as has_discord_server,
        post.website is not null as has_website,
        array_to_string(post.summary, E'\n') as text,
        coalesce((
            select json_object_agg(answer.key, answer.options)
            from (
                select field.key, json_agg(option.key order by option.ordinal) as options
                from post_field_option answer
                join field_option option on option.id = answer.field_option_id
                join field on field.id = option.field_id
                where answer.post_id = post.id
                group by field.key
            ) answer
        ), '{}') as fields,
        coalesce((
            select json_object_agg(field.key, json_build_array(from_option.key, to_option.key))
            from post_field_range range
            join field on field.id = range.field_id
            left join field_option from_option on from_option.id = range.from_option_id
            left join field_option to_option on to_option.id = range.to_option_id
            where range.post_id = post.id
        ), '{}') as ranges,
        coalesce((
            select json_agg(field.key)
            from post_field_flag flag
            join field on field.id = flag.field_id
            where flag.post_id = post.id
        ), '[]') as flags,
        row_number() over (partition by post.ilk = 'player' order by post.updated desc) as recency
    from post
    join player owner on owner.id = post.player_id
    where post.game_id = (select id from this_game)
)

select json_build_object(
    'handle', (select handle from this_game),
    'title', (select title from this_game),
    'now', :'now',
    'contacts', (
        select json_agg(kind order by kind)
        from game_contact where game_id = (select id from this_game)
    ),
    'trackers', (
        select coalesce(json_agg(json_build_object(
            'contact', contact_kind, 'title', title, 'template', template) order by id), '[]')
        from tracker where game_id = (select id from this_game)
    ),
    'fields', (
        select json_agg(json_build_object(
            'key', field.key,
            'label', field.label,
            'ilk', field.ilk,
            'ordered', field.ordered,
            'slotted', field.slotted,
            'appliesTo', field.applies_to,
            'onCard', field.on_card,
            'options', (
                select coalesce(json_agg(json_build_object('key', option.key, 'label', option.label) order by option.ordinal), '[]')
                from field_option option where option.field_id = field.id
            )
        ) order by field.ordinal)
        from field where field.game_id = (select id from this_game)
    ),
    'posts', (
        select coalesce(json_agg(to_jsonb(post) - 'recency' order by updated desc), '[]')
        from posts post
        where updated > :'now'::timestamptz - interval '1 year' or recency <= 60
    )
);
