-- One batch of a game's feed (brief 4, 7): its posts ordered by how well they fit
-- the viewer's description, each with the marks its card shows.
--
--   $1 text         the game's handle
--   $2 integer      the viewer, null when signed out
--   $3 jsonb        the viewer's description, below
--   $4 text[]       the post types shown: Showing's segment for a player, only
--                   'player' for a group or a community
--   $5 jsonb        the cursor of the last post shown, null for the first batch
--   $6 timestamptz  now
--
-- The description is the draft the bar writes, keyed by the game's field and
-- option keys; everything but the type may be left out:
--
--   { "type": "player",
--     "options": { "role": ["controller", "sentinel"], "rank": ["diamond-2"] },
--     "ranges": { "rank": { "from": "gold-1", "to": null } },
--     "flags": ["in-game-leader"],
--     "country": "Croatia", "age": 24,
--     "regions": ["Europe"], "ageFrom": 18, "ageTo": 30,
--     "languages": ["English"],
--     "online": { "from": "19:00", "to": "23:00" },
--     "timezone": "Europe/Zagreb",
--     "microphone": true }
--
-- options holds a player's point on an ordered field, and ranges a group's or
-- community's. A player gives a country and an age, a group or a community the
-- regions and ages it wants. timezone is the viewer's, which the online hours
-- are in; it is required with them.
--
-- Matching cannot be indexed: whether a post fits depends on the description,
-- so every post a batch may reach is compared and sorted. Active posts come
-- first, so a batch that stays among them compares only them; one that reaches
-- the expired posts compares every post in the game. What is compared and
-- sorted is kept narrow, a few columns per post, and the marks and the card are
-- built for the batch alone.
--
-- The cursor is the last row's, passed back as it came. It keeps a batch from
-- shifting when posts are renewed under it (brief 4), and counts the posts
-- shown so far, which says whether the next batch reaches the expired ones.

with viewer as (
    select
        viewer.*,
        -- The hours the viewer covers, as whole hours on a 24-hour circle: an
        -- arc of so many hours from its start. A range that ends where it
        -- starts covers the day.
        viewer.online_from / 60 as online_start,
        coalesce(nullif(((viewer.online_to + 59) / 60 - viewer.online_from / 60 + 24) % 24, 0), 24)
            as online_hours,
        -- The ages a post's owner may be, as the birthdays that give them, so
        -- no post's age is worked out: a player's within three years of their
        -- own, a group's or community's inside the range it gives.
        case when viewer.type = 'player' then $6::date - make_interval(years => viewer.age + 4)
            else $6::date - make_interval(years => viewer.age_to + 1)
        end as born_after,
        case when viewer.type = 'player' then $6::date - make_interval(years => viewer.age - 3)
            else $6::date - make_interval(years => viewer.age_from)
        end as born_by
    from (
        select
            $3->>'type' as type,
            (select region_name from country where name = $3->>'country') as region,
            ($3->>'age')::integer as age,
            array(select jsonb_array_elements_text(coalesce($3->'regions', '[]'))) as regions,
            ($3->>'ageFrom')::integer as age_from,
            ($3->>'ageTo')::integer as age_to,
            array(select jsonb_array_elements_text(coalesce($3->'languages', '[]'))) as languages,
            coalesce(($3->>'microphone')::boolean, false) as microphone,
            date_part('epoch', ($3->'online'->>'from')::time)::integer / 60 as online_from,
            date_part('epoch', ($3->'online'->>'to')::time)::integer / 60 as online_to,
            date_part('epoch', ($6 at time zone ($3->>'timezone')) - ($6 at time zone 'UTC'))::integer / 60
                as utc_offset
    ) viewer
),

this_game as (
    select id from game where handle = $1
),

-- Two players' ranks are near when they are within a tier of each other
-- (brief 7.2, Proposed). A tier is the options whose labels differ only in a
-- trailing division, and the game's commonest tier size is how many steps
-- that is; a ladder without divisions counts one.
tier as (
    select field.id as field_id, count(*) as size
    from field
    join field_option option on option.field_id = field.id
    where field.game_id = (select id from this_game) and field.ordered
    group by field.id, regexp_replace(option.label, '\s+(\d+|[IVX]+)$', '')
),

near as (
    select
        field_id,
        case when sum(often) = 1 then 1
            else (array_agg(size order by often desc, size desc))[1]
        end as steps
    from (select field_id, size, count(*) as often from tier group by field_id, size) sizes
    group by field_id
),

-- The game fields the description gives, as option ordinals: the options
-- chosen, a player's point as lo = hi, and a range with its open ends open.
-- Each field has a bit of its own, which is how a post's answers are counted.
described as (
    select
        field.id,
        1 << (row_number() over (order by field.ordinal) - 1)::integer as bit,
        field.key,
        field.ilk,
        field.ordered,
        field.slotted,
        field.applies_to,
        near.steps as near_steps,
        chosen.ordinals,
        case when field.ordered then
            case when viewer.type = 'player' then chosen.ordinals[1]
                else coalesce(range_from.ordinal, 0) end
        end as lo,
        case when field.ordered then
            case when viewer.type = 'player' then chosen.ordinals[1]
                else coalesce(range_to.ordinal, 2147483647) end
        end as hi
    from viewer
    join field on field.game_id = (select id from this_game)
        and viewer.type = any(field.applies_to)
    left join near on near.field_id = field.id
    left join lateral (
        select array_agg(option.ordinal order by option.ordinal) as ordinals
        from field_option option
        where option.field_id = field.id
            and option.key in (select jsonb_array_elements_text(coalesce($3->'options'->field.key, '[]')))
    ) chosen on true
    left join field_option range_from
        on range_from.field_id = field.id and range_from.key = $3->'ranges'->field.key->>'from'
    left join field_option range_to
        on range_to.field_id = field.id and range_to.key = $3->'ranges'->field.key->>'to'
    where case
        when field.ilk = 'boolean' then coalesce($3->'flags', '[]') ? field.key
        when field.ordered and viewer.type <> 'player' then
            range_from.id is not null or range_to.id is not null
        else chosen.ordinals is not null
    end
),

-- The described fields each post type is asked, as masks of their bits. A
-- field one of the two types isn't asked counts neither way.
asked as (
    select
        coalesce(bit_or(bit) filter (where 'player' = any(applies_to)), 0) as player,
        coalesce(bit_or(bit) filter (where 'group' = any(applies_to)), 0) as group_,
        coalesce(bit_or(bit) filter (where 'community' = any(applies_to)), 0) as community
    from described
),

-- A description that gives nothing shows every post by activity, the viewer's
-- own among them (brief 4).
description as (
    select not exists (select from described)
        and viewer.region is null and viewer.age is null
        and viewer.regions = '{}' and viewer.age_from is null and viewer.age_to is null
        and viewer.languages = '{}' and not viewer.microphone
        and (viewer.online_from is null or viewer.online_to is null)
        as empty
    from viewer
),

-- Every post the viewer may see.
visible as (
    select
        post.id,
        post.player_id,
        post.ilk,
        post.updated,
        post.updated <= $6 - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end as expired,
        post.regions,
        post.languages,
        post.age_from,
        post.age_to,
        post.microphone,
        post.online_from,
        post.online_to
    from post
    where post.game_id = (select id from this_game)
        and post.ilk = any($4)
        and not (post.player_id is not distinct from $2 and not (select empty from description))
        and ($2 is null or not exists (
            select from block
            where blocker_id = $2 and blocked_id = post.player_id
                or blocker_id = post.player_id and blocked_id = $2))
),

-- Active posts come first, so expired ones are compared only once a batch
-- reaches them: a game's posts are mostly expired, and a batch that more than
-- 20 active posts are left for never gets to them.
reach as (
    select
        coalesce(($5->>'expired')::boolean, false)
            or count(*) filter (where not expired) - coalesce(($5->>'shown')::integer, 0) <= 20
            as expired_reached
    from visible
),

-- The owners' timezones as offsets at now, worked out once for each rather
-- than for each post, and only when the viewer gives their hours.
zone as (
    select
        owner.timezone as name,
        date_part('epoch', ($6 at time zone owner.timezone) - ($6 at time zone 'UTC'))::integer / 60
            as utc_offset
    from visible
    join player owner on owner.id = visible.player_id
    cross join viewer
    where viewer.online_from is not null and owner.timezone is not null
        and (not visible.expired or (select expired_reached from reach))
    group by owner.timezone
),

-- The posts a batch may reach, with their marks on the account's facts, which
-- a group and a community ask of the players they want alike (brief 7.2).
-- Each is 'fit', 'miss', or 'missing' where the post left it empty, which
-- counts as a miss, and null where the viewer didn't give it.
candidate as (
    select
        post.id,
        post.player_id,
        post.ilk,
        post.updated,
        post.expired,

        case when viewer.online_from is not null and viewer.online_to is not null then
            case when post.online_from is null or post.online_to is null then 'missing'
                -- Two arcs overlap when either starts inside the other.
                when mod(theirs.online_start - viewer.online_start + 24, 24) < viewer.online_hours
                    or mod(viewer.online_start - theirs.online_start + 24, 24) < theirs.online_hours
                then 'fit' else 'miss' end
        end as hours_mark,

        -- A microphone is compared only where the viewer gave one: a player
        -- says they use one, a group or a community that it wants one.
        case when viewer.microphone then
            case when post.microphone then 'fit' else 'miss' end
        end as mic_mark,

        case when viewer.languages <> '{}' then
            case when languages.spoken = '{}' then 'missing'
                when languages.spoken && viewer.languages then 'fit' else 'miss' end
        end as languages_mark,

        -- A player's country is compared through its region.
        case
            when viewer.type = 'player' and viewer.region is not null then
                case when post.ilk = 'player' then
                    case when country.region_name is null then 'missing'
                        when country.region_name = viewer.region then 'fit' else 'miss' end
                else
                    case when post.regions = '{}' then 'missing'
                        when viewer.region = any(post.regions) then 'fit' else 'miss' end
                end
            when viewer.type <> 'player' and post.ilk = 'player' and viewer.regions <> '{}' then
                case when country.region_name is null then 'missing'
                    when country.region_name = any(viewer.regions) then 'fit' else 'miss' end
        end as location_mark,

        -- Two players' ages are near within three years; a player's is inside a
        -- group's or community's range.
        case
            when viewer.type = 'player' and viewer.age is not null and post.ilk <> 'player' then
                case when post.age_from is null and post.age_to is null then 'missing'
                    when viewer.age between coalesce(post.age_from, 0) and coalesce(post.age_to, 1000)
                    then 'fit' else 'miss' end
            when post.ilk = 'player' and (viewer.type = 'player' and viewer.age is not null
                or viewer.type <> 'player' and (viewer.age_from is not null or viewer.age_to is not null)) then
                case when owner.birthday is null then 'missing'
                    when owner.birthday > coalesce(viewer.born_after, '-infinity')
                        and owner.birthday <= coalesce(viewer.born_by, 'infinity')
                    then 'fit' else 'miss' end
        end as age_mark
    from visible post
    cross join viewer
    join player owner on owner.id = post.player_id
    left join country on country.name = owner.country
    cross join lateral (
        select case when post.ilk = 'player' then owner.languages else post.languages end as spoken
    ) languages
    left join zone on zone.name = owner.timezone
    -- The post's hours moved into the viewer's timezone, as an arc like the
    -- viewer's. Hours with no timezone are taken to be the viewer's.
    cross join lateral (
        select
            shifted.online_from / 60 as online_start,
            coalesce(nullif(((shifted.online_to + 59) / 60 - shifted.online_from / 60 + 24) % 24, 0), 24)
                as online_hours
        from (
            select
                ((date_part('epoch', post.online_from)::integer / 60 + shift.minutes) % 1440 + 1440) % 1440
                    as online_from,
                ((date_part('epoch', post.online_to)::integer / 60 + shift.minutes) % 1440 + 1440) % 1440
                    as online_to
            from (
                select viewer.utc_offset - coalesce(zone.utc_offset, viewer.utc_offset) as minutes
            ) shift
        ) shifted
    ) theirs
    where not post.expired or (select expired_reached from reach)
),

-- The options the candidates chose for the described fields. How many posts a
-- batch compares is known only once it runs, so the plan can't pick how to
-- find them: each way is behind its own check. A batch that doesn't reach the
-- expired posts looks up its few posts one by one, and one that does reads
-- the described fields' options for the whole game.
chosen as (
    select answer.post_id, answer.field_option_id
    from candidate
    join post_field_option answer on answer.post_id = candidate.id
    where not (select expired_reached from reach)
    union all
    select answer.post_id, answer.field_option_id
    from described
    join field_option option on option.field_id = described.id
    join post_field_option answer on answer.field_option_id = option.id
    where (select expired_reached from reach)
),

-- The candidates' answers to the described fields, a row per option chosen,
-- range given or yes, each saying whether it fits (brief 7.2). Two players
-- compare differently from a player and a group, so a row says both ways, and
-- the post's type picks one once it is known.
answer as (
    select
        chosen.post_id,
        described.bit,
        -- Two players' ranks are near, and on a slotted field they fit when
        -- between them they cover two different slots: all that misses is
        -- both locked to the same one.
        case
            when described.ordered then abs(described.lo - option.ordinal) <= described.near_steps
            when described.slotted then
                cardinality(described.ordinals) > 1 or option.ordinal <> described.ordinals[1]
            else option.ordinal = any(described.ordinals)
        end as fits_players,
        -- Otherwise a point is inside the range, and anything else fits on a
        -- shared option.
        case
            when described.ordered then option.ordinal between described.lo and described.hi
            else option.ordinal = any(described.ordinals)
        end as fits
    from chosen
    join field_option option on option.id = chosen.field_option_id
    join described on described.id = option.field_id
    union all
    select range.post_id, described.bit, fits, fits
    from post_field_range range
    join described on described.id = range.field_id
    left join field_option range_from on range_from.id = range.from_option_id
    left join field_option range_to on range_to.id = range.to_option_id
    cross join lateral (
        select described.lo <= coalesce(range_to.ordinal, 2147483647)
            and coalesce(range_from.ordinal, 0) <= described.hi as fits
    ) range_fits
    union all
    select flag.post_id, described.bit, true, true
    from post_field_flag flag
    join described on described.id = flag.field_id
),

-- A field fits when any of the post's rows for it does, so a post's answers
-- fold into masks of the described fields' bits: the fields it answered and
-- the fields that fit.
answered as (
    select
        post_id,
        bit_or(bit) as answered,
        bit_or(case when fits_players then bit else 0 end) as fitted_players,
        bit_or(case when fits then bit else 0 end) as fitted
    from answer
    group by post_id
),

-- A tier counts the misses; a post none of the description applies to goes
-- last, which with an empty description is every post, leaving activity to
-- order them. The headings group every miss past the first (brief 7.2).
ranked as (
    select
        candidate.*,
        fields.asked,
        coalesce(answered.answered, 0) as answered,
        fields.fitted,
        counts.compared,
        counts.misses,
        case when counts.compared = 0 then 2147483647 else counts.misses end as sort_misses
    from candidate
    cross join viewer
    cross join asked
    left join answered on answered.post_id = candidate.id
    cross join lateral (
        select
            case candidate.ilk
                when 'player' then asked.player
                when 'group' then asked.group_
                else asked.community
            end as asked,
            case when viewer.type = 'player' and candidate.ilk = 'player'
                then coalesce(answered.fitted_players, 0)
                else coalesce(answered.fitted, 0)
            end as fitted
    ) fields
    cross join lateral (
        select
            bit_count(fields.asked::bit(32)) + num_nonnulls(
                hours_mark, mic_mark, languages_mark, location_mark, age_mark)
                as compared,
            bit_count((fields.asked & ~fields.fitted)::bit(32)) + num_nonnulls(
                nullif(hours_mark, 'fit'), nullif(mic_mark, 'fit'), nullif(languages_mark, 'fit'),
                nullif(location_mark, 'fit'), nullif(age_mark, 'fit'))
                as misses
    ) counts
),

tiers as (
    select
        count(*) filter (where not expired and compared > 0 and misses = 0) as fits_count,
        count(*) filter (where not expired and compared > 0 and misses = 1) as missing_one_count,
        count(*) filter (where not expired and (compared = 0 or misses > 1)) as missing_more_count
    from ranked
),

-- Active posts first, then expired ones, each in tiers and by activity inside
-- a tier (brief 4). A batch starts after the cursor's post, and takes one post
-- more than it shows to tell whether more follow.
batch as (
    select ranked.*
    from ranked
    where $5 is null
        or (expired, sort_misses) > (($5->>'expired')::boolean, ($5->>'misses')::integer)
        or ((expired, sort_misses) = (($5->>'expired')::boolean, ($5->>'misses')::integer)
            and (updated, id) < (($5->>'updated')::timestamptz, ($5->>'id')::integer))
    order by expired, sort_misses, updated desc, id desc
    limit 21
)


select
    batch.id,
    post.ilk as type,
    post.name,
    owner.nickname as owner,
    coalesce(batch.player_id = $2, false) as own,
    -- Times come as ISO strings, as they are in JSON, so the server reads a
    -- row as the client does. When the viewer first wrote about the post, or
    -- null where they haven't.
    (
        select to_jsonb(min(message.created))
        from conversation
        join message on message.conversation_id = conversation.id
        where conversation.post_id = batch.id and conversation.messager_id = $2
            and message.sender_id = $2
    ) as messaged,
    to_jsonb(batch.updated) as updated,
    batch.expired,
    post.summary,
    case when post.ilk = 'player' then date_part('year', age($6, owner.birthday)) end as age,
    case when post.ilk = 'player' then owner.country end as country,
    case when post.ilk = 'player' then owner.languages else post.languages end as languages,
    post.regions,
    post.age_from,
    post.age_to,
    post.group_size,
    post.group_wanted_from,
    post.group_wanted_to,
    owner.timezone,
    to_char(post.online_from, 'HH24:MI') as online_from,
    to_char(post.online_to, 'HH24:MI') as online_to,
    post.microphone,
    post.contact_preference,
    -- Which contacts the post offers, never the contacts themselves: those are
    -- revealed from the contact panel, which counts it (brief 11.2). A game
    -- account a tracker links is the exception, below.
    array(
        select kind from game_contact
        where game_contact.game_id = post.game_id
            and post.ilk <> 'community'
            and case kind
                when 'discord' then owner.discord_tag
                when 'steam' then owner.steam_id
                when 'riot' then owner.riot_id
                when 'battle_tag' then owner.battle_tag
                when 'ea' then owner.ea_id
                when 'ubisoft' then owner.ubisoft_username
                when 'psn' then owner.psn_id
                when 'gamer_tag' then owner.gamer_tag
                when 'friend_code' then owner.friend_code
            end is not null
        order by kind
    ) as contacts,
    -- A player post's trackers, each with the owner's account its template
    -- takes, so the card links their profiles behind Details (brief 5.4).
    coalesce((
        select jsonb_agg(jsonb_build_object(
            'title', tracker.title, 'template', tracker.template, 'account', account
        ) order by tracker.id)
        from tracker
        cross join lateral (select case tracker.contact_kind
            when 'discord' then owner.discord_tag
            when 'steam' then owner.steam_id
            when 'riot' then owner.riot_id
            when 'battle_tag' then owner.battle_tag
            when 'ea' then owner.ea_id
            when 'ubisoft' then owner.ubisoft_username
            when 'psn' then owner.psn_id
            when 'gamer_tag' then owner.gamer_tag
            when 'friend_code' then owner.friend_code
        end as account) accounts
        where tracker.game_id = post.game_id
            and post.ilk = 'player'
            and account is not null
    ), '[]') as trackers,
    post.discord_server is not null as has_discord_server,
    post.website is not null as has_website,
    coalesce((
        select jsonb_object_agg(field.key, options)
        from (
            select option.field_id, jsonb_agg(option.key order by option.ordinal) as options
            from post_field_option answer
            join field_option option on option.id = answer.field_option_id
            where answer.post_id = batch.id
            group by option.field_id
        ) answers
        join field on field.id = answers.field_id
    ), '{}') as options,
    coalesce((
        select jsonb_object_agg(field.key, jsonb_build_object('from', range_from.key, 'to', range_to.key))
        from post_field_range range
        join field on field.id = range.field_id
        left join field_option range_from on range_from.id = range.from_option_id
        left join field_option range_to on range_to.id = range.to_option_id
        where range.post_id = batch.id
    ), '{}') as ranges,
    array(
        select field.key
        from post_field_flag flag
        join field on field.id = flag.field_id
        where flag.post_id = batch.id
        order by field.ordinal
    ) as flags,
    -- The marks the card shows, keyed by the fact or the game field's key.
    jsonb_strip_nulls(jsonb_build_object(
        'hours', batch.hours_mark,
        'mic', batch.mic_mark,
        'languages', batch.languages_mark,
        'location', batch.location_mark,
        'age', case when post.ilk = 'player' then batch.age_mark end,
        'ages', case when post.ilk <> 'player' then batch.age_mark end
    )) || coalesce((
        -- A field the post left empty is missing, but a boolean is never
        -- empty: no row is its no.
        select jsonb_object_agg(described.key, case
            when batch.fitted & described.bit <> 0 then 'fit'
            when described.ilk <> 'boolean' and batch.answered & described.bit = 0 then 'missing'
            else 'miss'
        end)
        from described
        where batch.asked & described.bit <> 0
    ), '{}') as marks,
    batch.compared,
    batch.misses,
    tiers.fits_count,
    tiers.missing_one_count,
    tiers.missing_more_count,
    (select count(*) from batch) > 20 as more,
    jsonb_build_object(
        'expired', batch.expired, 'misses', batch.sort_misses,
        'updated', batch.updated, 'id', batch.id,
        'shown', coalesce(($5->>'shown')::integer, 0)
            + row_number() over (order by batch.expired, batch.sort_misses, batch.updated desc, batch.id desc)
    ) as cursor
from (
    select * from batch
    order by expired, sort_misses, updated desc, id desc
    limit 20
) batch
cross join tiers
join post on post.id = batch.id
join player owner on owner.id = batch.player_id
order by batch.expired, batch.sort_misses, batch.updated desc, batch.id desc
