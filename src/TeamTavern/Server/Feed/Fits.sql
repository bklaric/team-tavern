-- The posts one post fits, each judged from its owner's seat (brief 7.2, 8):
-- the other post as the description its owner's feed would take, and this post
-- as the one candidate. It fits when it misses nothing that description gives,
-- which is what the feed's Fits you counts, so these are the owners a new post
-- tells.
--
--   $1 integer      the post
--   $2 timestamptz  now
--
-- It is Feed.sql turned round: the feed compares one description with a game's
-- posts, this compares a game's posts, each as a description, with one post.
-- Every part names the part of Feed.sql it follows, and a change to how two
-- posts compare is made in both. A post's description is read from its rows as
-- descriptionJson (ViewOwnDescriptions.purs) writes it, not from JSON.
--
-- Only active posts are told, and only of an active post. A group or a
-- community is told of players alone, since its feed shows it only them, and a
-- block keeps either side from being told of the other.

-- node-pg sends the parameters untyped, and Postgres gives each the type it is
-- first read as, so they are read here, before anything else reads them.
with parameters as (
    select $1::integer, $2::timestamptz
),

-- The candidate with its owner's facts (Feed.sql: visible, candidate).
this_post as (
    select
        post.id,
        post.player_id,
        post.game_id,
        post.ilk,
        post.regions,
        post.age_from,
        post.age_to,
        post.microphone,
        post.online_from,
        post.online_to,
        country.region_name as region,
        owner.birthday,
        case when post.ilk = 'player' then owner.languages else post.languages end as spoken,
        date_part('epoch', ($2 at time zone owner.timezone) - ($2 at time zone 'UTC'))::integer / 60
            as utc_offset
    from post
    join player owner on owner.id = post.player_id
    left join country on country.name = owner.country
    where post.id = $1
        and post.updated > $2 - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end
),

-- The posts that may be told, each with the facts its description gives
-- (Feed.sql: viewer, visible).
seat as (
    select
        seat.*,
        seat.online_from / 60 as online_start,
        coalesce(nullif(((seat.online_to + 59) / 60 - seat.online_from / 60 + 24) % 24, 0), 24)
            as online_hours,
        case when seat.type = 'player' then $2::date - make_interval(years => seat.age + 4)
            else $2::date - make_interval(years => seat.age_to + 1)
        end as born_after,
        case when seat.type = 'player' then $2::date - make_interval(years => seat.age - 3)
            else $2::date - make_interval(years => seat.age_from)
        end as born_by
    from (
        select
            post.id,
            post.ilk as type,
            case when post.ilk = 'player' then country.region_name end as region,
            case when post.ilk = 'player' then date_part('year', age($2, owner.birthday))::integer end
                as age,
            post.regions,
            post.age_from,
            post.age_to,
            case when post.ilk = 'player' then owner.languages else post.languages end as languages,
            post.microphone,
            case when post.online_from is not null and post.online_to is not null
                then date_part('epoch', post.online_from)::integer / 60 end as online_from,
            case when post.online_from is not null and post.online_to is not null
                then date_part('epoch', post.online_to)::integer / 60 end as online_to,
            date_part('epoch', ($2 at time zone owner.timezone) - ($2 at time zone 'UTC'))::integer / 60
                as utc_offset
        from this_post
        join post on post.game_id = this_post.game_id
            and post.player_id <> this_post.player_id
            and (post.ilk = 'player' or this_post.ilk = 'player')
            and post.updated > $2 - case when post.ilk = 'community'
                then interval '90 days' else interval '30 days' end
        join player owner on owner.id = post.player_id
        left join country on country.name = owner.country
        where not exists (
            select from block
            where blocker_id = this_post.player_id and blocked_id = post.player_id
                or blocker_id = post.player_id and blocked_id = this_post.player_id)
    ) seat
),

-- The game's fields, each with a bit of its own (Feed.sql: described).
game_field as (
    select field.*, 1 << (row_number() over (order by field.ordinal) - 1)::integer as bit
    from field
    where field.game_id = (select game_id from this_post)
),

-- Feed.sql: tier.
tier as (
    select field.id as field_id, count(*) as size
    from game_field field
    join field_option option on option.field_id = field.id
    where field.ordered
    group by field.id, regexp_replace(option.label, '\s+(\d+|[IVX]+)$', '')
),

-- Feed.sql: near.
near as (
    select
        field_id,
        case when sum(often) = 1 then 1
            else (array_agg(size order by often desc, size desc))[1]
        end as steps
    from (select field_id, size, count(*) as often from tier group by field_id, size) sizes
    group by field_id
),

-- The game fields each seat's post gives, read from its answers (Feed.sql:
-- described).
described as (
    select
        seat.id as seat_id,
        field.id,
        field.bit,
        field.ilk,
        field.ordered,
        field.slotted,
        field.applies_to,
        field.ilk = 'boolean' and flag.field_id is not null as said,
        near.steps as near_steps,
        chosen.ordinals,
        case when field.ordered then
            case when seat.type = 'player' then chosen.ordinals[1]
                else coalesce(range_from.ordinal, 0) end
        end as lo,
        case when field.ordered then
            case when seat.type = 'player' then chosen.ordinals[1]
                else coalesce(range_to.ordinal, 2147483647) end
        end as hi
    from seat
    join game_field field on seat.type = any(field.applies_to)
    left join near on near.field_id = field.id
    left join post_field_flag flag on flag.post_id = seat.id and flag.field_id = field.id
    left join lateral (
        select array_agg(option.ordinal order by option.ordinal) as ordinals
        from post_field_option answer
        join field_option option on option.id = answer.field_option_id
        where answer.post_id = seat.id and option.field_id = field.id
    ) chosen on true
    left join post_field_range range on range.post_id = seat.id and range.field_id = field.id
    left join field_option range_from on range_from.id = range.from_option_id
    left join field_option range_to on range_to.id = range.to_option_id
    where case
        when field.ilk = 'boolean' then seat.type = 'player' or flag.field_id is not null
        when field.ordered and seat.type <> 'player' then
            range_from.id is not null or range_to.id is not null
        else chosen.ordinals is not null
    end
),

-- Feed.sql: asked.
asked as (
    select
        seat.id as seat_id,
        coalesce(bit_or(bit) filter (where 'player' = any(applies_to)), 0) as player,
        coalesce(bit_or(bit) filter (where 'group' = any(applies_to)), 0) as group_,
        coalesce(bit_or(bit) filter (where 'community' = any(applies_to)), 0) as community,
        coalesce(bit_or(bit) filter (where ilk = 'boolean'), 0) as flags,
        coalesce(bit_or(bit) filter (where said), 0) as said
    from seat
    left join described on described.seat_id = seat.id
    group by seat.id
),

-- Feed.sql: description.
description as (
    select
        seat.id as seat_id,
        not exists (select from described where described.seat_id = seat.id and (ilk <> 'boolean' or said))
            and seat.region is null and seat.age is null
            and seat.regions = '{}' and seat.age_from is null and seat.age_to is null
            and seat.languages = '{}' and not seat.microphone
            and (seat.online_from is null or seat.online_to is null)
            as empty
    from seat
),

-- The candidate's marks on the account's facts from each seat (Feed.sql:
-- candidate).
marked as (
    select
        seat.id as seat_id,
        seat.type,

        case when seat.online_from is not null and seat.online_to is not null then
            case when post.online_from is null or post.online_to is null then 'missing'
                when mod(theirs.online_start - seat.online_start + 24, 24) < seat.online_hours
                    or mod(seat.online_start - theirs.online_start + 24, 24) < theirs.online_hours
                then 'fit' else 'miss' end
        end as hours_mark,

        case when seat.microphone then
            case when post.microphone then 'fit' else 'miss' end
        end as mic_mark,

        case when seat.languages <> '{}' then
            case when post.spoken = '{}' then 'missing'
                when post.spoken && seat.languages then 'fit' else 'miss' end
        end as languages_mark,

        case
            when seat.type = 'player' and seat.region is not null then
                case when post.ilk = 'player' then
                    case when post.region is null then 'missing'
                        when post.region = seat.region then 'fit' else 'miss' end
                else
                    case when post.regions = '{}' then 'missing'
                        when seat.region = any(post.regions) then 'fit' else 'miss' end
                end
            when seat.type <> 'player' and post.ilk = 'player' and seat.regions <> '{}' then
                case when post.region is null then 'missing'
                    when post.region = any(seat.regions) then 'fit' else 'miss' end
        end as location_mark,

        case
            when seat.type = 'player' and seat.age is not null and post.ilk <> 'player' then
                case when post.age_from is null and post.age_to is null then 'missing'
                    when seat.age between coalesce(post.age_from, 0) and coalesce(post.age_to, 1000)
                    then 'fit' else 'miss' end
            when post.ilk = 'player' and (seat.type = 'player' and seat.age is not null
                or seat.type <> 'player' and (seat.age_from is not null or seat.age_to is not null)) then
                case when post.birthday is null then 'missing'
                    when post.birthday > coalesce(seat.born_after, '-infinity')
                        and post.birthday <= coalesce(seat.born_by, 'infinity')
                    then 'fit' else 'miss' end
        end as age_mark
    from seat
    cross join this_post post
    -- The candidate's hours moved into the seat's timezone.
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
                select seat.utc_offset - coalesce(post.utc_offset, seat.utc_offset) as minutes
            ) shift
        ) shifted
    ) theirs
),

-- The candidate's answers to each seat's described fields (Feed.sql: answer).
answer as (
    select
        described.seat_id,
        described.bit,
        case
            when described.ordered then abs(described.lo - option.ordinal) <= described.near_steps
            when described.slotted then
                cardinality(described.ordinals) > 1 or option.ordinal <> described.ordinals[1]
            else option.ordinal = any(described.ordinals)
        end as fits_players,
        case
            when described.ordered then option.ordinal between described.lo and described.hi
            else option.ordinal = any(described.ordinals)
        end as fits
    from post_field_option chosen
    join field_option option on option.id = chosen.field_option_id
    join described on described.id = option.field_id
    where chosen.post_id = $1
    union all
    select described.seat_id, described.bit, fits, fits
    from post_field_range range
    join described on described.id = range.field_id
    left join field_option range_from on range_from.id = range.from_option_id
    left join field_option range_to on range_to.id = range.to_option_id
    cross join lateral (
        select described.lo <= coalesce(range_to.ordinal, 2147483647)
            and coalesce(range_from.ordinal, 0) <= described.hi as fits
    ) range_fits
    where range.post_id = $1
    union all
    select described.seat_id, described.bit, true, described.said
    from post_field_flag flag
    join described on described.id = flag.field_id
    where flag.post_id = $1
),

-- Feed.sql: answered.
answered as (
    select
        seat_id,
        bit_or(bit) as answered,
        bit_or(case when fits_players then bit else 0 end) as fitted_players,
        bit_or(case when fits then bit else 0 end) as fitted
    from answer
    group by seat_id
),

-- Feed.sql: ranked.
ranked as (
    select marked.seat_id, counts.compared, counts.misses
    from marked
    cross join this_post candidate
    join asked on asked.seat_id = marked.seat_id
    join description on description.seat_id = marked.seat_id
    left join answered on answered.seat_id = marked.seat_id
    cross join lateral (
        select
            case
                when description.empty then 0
                when candidate.ilk = 'player' then asked.player & ~(asked.flags & ~asked.said & ~coalesce(answered.answered, 0))
                when candidate.ilk = 'group' then asked.group_ & ~(asked.flags & ~coalesce(answered.answered, 0))
                else asked.community & ~(asked.flags & ~coalesce(answered.answered, 0))
            end as asked,
            case when marked.type = 'player' and candidate.ilk = 'player'
                then coalesce(answered.fitted_players, 0) | asked.said
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
)

select seat_id as id
from ranked
where compared > 0 and misses = 0
order by seat_id
