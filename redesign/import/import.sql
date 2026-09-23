-- Imports the production dump into the new schema (brief 12). import.sh restores
-- the dump into the legacy schema and runs mapping.sql and this in one
-- transaction; the legacy tables are only read.
--
-- What doesn't convert is dropped, and every step that drops something keeps
-- the reason beside the rows it read, for report.sql.

-- A mapping that names a field or option the catalogue doesn't have would drop
-- its answers without saying so, so it stops the import instead.
do $$
declare
    broken text;
begin
    select string_agg(format('%s %s.%s -> %s.%s..%s', m.game, m.old_field, m.old_option,
        m.new_field, m.new_from, coalesce(m.new_to, m.new_from)), E'\n')
    into broken
    from legacy.option_map m
    left join game on game.handle = m.game
    left join field on field.game_id = game.id and field.key = m.new_field
    left join field_option from_option on from_option.field_id = field.id and from_option.key = m.new_from
    left join field_option to_option on to_option.field_id = field.id
        and to_option.key = coalesce(m.new_to, m.new_from)
    left join legacy.game_map on game_map.new_handle = m.game
    left join legacy.game old_game on old_game.handle = game_map.old_handle
    left join legacy.field old_field on old_field.game_id = old_game.id and old_field.key = m.old_field
    left join legacy.field_option old_option on old_option.field_id = old_field.id and old_option.key = m.old_option
    where field.id is null
        or (field.ilk = 'boolean') <> (m.new_from is null)
        or (field.ilk <> 'boolean' and (from_option.id is null or to_option.id is null))
        or (old_option.id is null and m.old_field <> 'new-or-returning')
        or (m.new_to is not null and not field.ordered);
    if broken is not null then
        raise exception E'option_map rows that name nothing, name an option of a boolean, or span an unordered field:\n%', broken;
    end if;
end $$;

-- Every answer a mapping can give, with the new ids: option_map's rows, and a
-- platform wherever the game has a platform field with that option. A boolean
-- field's row names no option.
create table legacy.answer_map as
select game.id as game_id, m.old_field, m.old_option, field.id as field_id,
    from_option.id as from_option_id, from_option.ordinal as from_ordinal,
    to_option.ordinal as to_ordinal
from legacy.option_map m
join game on game.handle = m.game
join field on field.game_id = game.id and field.key = m.new_field
left join field_option from_option on from_option.field_id = field.id and from_option.key = m.new_from
left join field_option to_option on to_option.field_id = field.id
    and to_option.key = coalesce(m.new_to, m.new_from)
union all
select field.game_id, 'platform', platform_map.old_platform, field.id,
    option.id, option.ordinal, option.ordinal
from legacy.platform_map
join field_option option on option.key = platform_map.new_platform
join field on field.id = option.field_id and field.key = 'platform';

-- Today's leaves are its countries; the inner nodes are regions of a tree the
-- new schema doesn't have.
create table legacy.location_country as
select leaf.name as old_name, country.name as country_name
from legacy.region leaf
left join legacy.country_map on country_map.old_name = leaf.name
join country on country.name = coalesce(country_map.new_name, leaf.name)
where not exists (select 1 from legacy.region child where child.superregion_name = leaf.name);

-- Players, with their ids, so the posts and sessions below keep pointing at
-- them. A location carries over only where it names a country.

insert into player
    ( id, nickname, email, password_hash, discord_id
    , birthday, languages, country, timezone
    , discord_tag, steam_id, riot_id, battle_tag, ea_id
    , ubisoft_username, psn_id, gamer_tag, friend_code
    , registered
    )
select old.id, old.nickname, old.email, old.password_hash, old.discord_id,
    old.birthday, old.languages, location_country.country_name, old.timezone,
    old.discord_tag, old.steam_id, old.riot_id, old.battle_tag, old.ea_id,
    old.ubisoft_username, old.psn_id, old.gamer_tag, old.friend_code,
    old.registered
from legacy.player old
left join legacy.location_country on location_country.old_name = old.location;

select setval('player_id_seq', (select max(id) from player));

-- The new schema keeps a SHA-256 of each token, the hash the server takes of
-- the token a browser sends. The dump doesn't say when a session was last
-- used, so each starts its year at the import.
insert into session (player_id, token_hash, revoked, generated)
select player_id, encode(sha256(convert_to(token, 'UTF8')), 'hex'), revoked, generated
from legacy.session
where not revoked;

-- Every profile, player and team alike, with what its post is made from. A
-- team profile's hours, microphone and details are the team's; a player
-- profile's are the player's.

create table legacy.post_source as
with source as (
    select 'player profile' as source, profile.id as source_id, 'player' as ilk,
        profile.player_id, profile.game_id as old_game_id,
        profile.about, profile.ambitions, profile.created, profile.updated,
        array[profile.platform] as platforms, profile.new_or_returning,
        player.microphone, player.timezone,
        player.weekday_from, player.weekday_to, player.weekend_from, player.weekend_to,
        null::text as name, null::text as website, null::text as discord_server,
        '{}'::text[] as locations, '{}'::text[] as languages,
        null::integer as age_from, null::integer as age_to
    from legacy.player_profile profile
    join legacy.player on player.id = profile.player_id
    union all
    select 'team profile', profile.id,
        case profile.size when 'community' then 'community' else 'group' end,
        team.owner_id, profile.game_id,
        profile.about, profile.ambitions, profile.created, profile.updated,
        profile.platforms, profile.new_or_returning,
        team.microphone, team.timezone,
        team.weekday_from, team.weekday_to, team.weekend_from, team.weekend_to,
        -- A community has to have a name, and an unnamed team was shown by its
        -- handle, which is its owner's nickname.
        case profile.size
            when 'community' then coalesce(nullif(trim(team.name), ''), team.handle)
            else nullif(trim(team.name), '')
        end,
        team.website, team.discord_server,
        team.locations, team.languages,
        team.age_from, team.age_to
    from legacy.team_profile profile
    join legacy.team on team.id = profile.team_id
),
summarised as (
    select source.*, game.id as game_id,
        -- About and ambitions become one text; ambitions that about already
        -- quotes are not repeated.
        case when strpos(array_to_string(source.about, E'\n'), array_to_string(source.ambitions, E'\n')) > 0
            then source.about
            else source.about || source.ambitions
        end as summary
    from source
    join legacy.game old_game on old_game.id = source.old_game_id
    left join legacy.game_map on game_map.old_handle = old_game.handle
    left join game on game.handle = game_map.new_handle
),
placed as (
    select summarised.*,
        -- A player has one post of a type per game, and an owner of several
        -- teams may have had several profiles of a size in one; the most
        -- recently updated one that converts is the one kept.
        row_number() over (
            partition by player_id, game_id, ilk
            order by (ilk <> 'community' or array_to_string(summary, '') ~ '\S') desc, updated desc
        ) as place
    from summarised
)
select placed.*,
    case
        when game_id is null then 'game not in the catalogue'
        when ilk = 'community' and array_to_string(summary, '') !~ '\S' then 'community with no text'
        when place > 1 then 'owner has a newer post of the type in the game'
    end as dropped_because,
    null::integer as post_id
from placed;

update legacy.post_source
set post_id = nextval('post_id_seq')
where dropped_because is null;

-- Posts keep their hours in the owner's timezone, and a team kept its own, so a
-- team's hours move into its owner's. The dump's date stands for the offsets.
create function pg_temp.in_owner_time(t time, source_timezone text, owner_timezone text)
returns time language sql immutable as $$
    select case
        when source_timezone is null or owner_timezone is null then t
        else ((date '2026-09-12' + t) at time zone source_timezone at time zone owner_timezone)::time
    end
$$;

insert into post
    ( id, player_id, game_id, ilk, updated, created, renewal_nonce, summary
    , microphone, online_from, online_to, contact_preference
    , name, regions, languages, website, discord_server, age_from, age_to
    )
select source.post_id, source.player_id, source.game_id, source.ilk,
    source.updated, source.created,
    substr(md5(gen_random_uuid()::text), 1, 20),
    source.summary,
    source.microphone,
    pg_temp.in_owner_time(hours.online_from, source.timezone, owner.timezone),
    pg_temp.in_owner_time(hours.online_to, source.timezone, owner.timezone),
    case
        when source.ilk <> 'community' then
            case when owner.discord_tag is not null then 'offsite' else 'message' end
        when source.discord_server is not null then 'discord'
        when source.website is not null then 'website'
        else 'message'
    end,
    source.name,
    (select coalesce(array_agg(region.name order by region.ordinal), '{}')
        from region
        where region.name in (
            select map.region_name
            from unnest(source.locations) location
            join legacy.location_region_map map on map.old_name = location
            union
            select country.region_name
            from unnest(source.locations) location
            join legacy.location_country on location_country.old_name = location
            join country on country.name = location_country.country_name)),
    source.languages, source.website, source.discord_server,
    source.age_from, source.age_to
from legacy.post_source source
join player owner on owner.id = source.player_id
cross join lateral (
    select
        case when source.weekday_from is not null and source.weekday_to is not null
            then source.weekday_from else source.weekend_from end as online_from,
        case when source.weekday_from is not null and source.weekday_to is not null
            then source.weekday_to else source.weekend_to end as online_to
) hours
where source.post_id is not null;

-- Game field answers.

create table legacy.post_answer as
with answer as (
    select source.post_id, source.ilk, source.game_id, old_field.key as old_field, old_option.key as old_option
    from legacy.post_source source
    join legacy.player_profile_field_value value
        on source.source = 'player profile' and value.player_profile_id = source.source_id
    join legacy.player_profile_field_value_option value_option
        on value_option.player_profile_field_value_id = value.id
    join legacy.field old_field on old_field.id = value.field_id
    join legacy.field_option old_option on old_option.id = value_option.field_option_id
    where source.post_id is not null
    union all
    select source.post_id, source.ilk, source.game_id, old_field.key, old_option.key
    from legacy.post_source source
    join legacy.team_profile_field_value value
        on source.source = 'team profile' and value.team_profile_id = source.source_id
    join legacy.team_profile_field_value_option value_option
        on value_option.team_profile_field_value_id = value.id
    join legacy.field old_field on old_field.id = value.field_id
    join legacy.field_option old_option on old_option.id = value_option.field_option_id
    where source.post_id is not null
    union all
    select source.post_id, source.ilk, source.game_id, 'platform', platform
    from legacy.post_source source
    cross join unnest(source.platforms) platform
    where source.post_id is not null
    union all
    select source.post_id, source.ilk, source.game_id, 'new-or-returning', 'new-or-returning'
    from legacy.post_source source
    where source.post_id is not null and source.new_or_returning
)
select answer.*, map.field_id, field.ilk as field_ilk, field.ordered, map.from_option_id, map.from_ordinal, map.to_ordinal,
    case
        when map.field_id is null and answer.old_field = 'platform' and not exists
            (select 1 from field platform where platform.game_id = answer.game_id and platform.key = 'platform')
            then 'game has no platform field'
        when map.field_id is null then 'no mapping'
        when not answer.ilk = any(field.applies_to) then 'not asked of a ' || answer.ilk
    end as dropped_because
from answer
left join legacy.answer_map map
    on map.game_id = answer.game_id
    and map.old_field = answer.old_field
    and map.old_option = answer.old_option
left join field on field.id = map.field_id;

insert into post_field_option (post_id, field_option_id)
select distinct post_id, from_option_id
from legacy.post_answer
where dropped_because is null and not ordered and field_ilk <> 'boolean';

insert into post_field_flag (post_id, field_id)
select distinct post_id, field_id
from legacy.post_answer
where dropped_because is null and field_ilk = 'boolean';

-- An ordered field's answers are one span: a group's range, and for a player
-- the option in the middle of it.
create table legacy.post_span as
select post_id, ilk, field_id,
    min(least(from_ordinal, to_ordinal)) as from_ordinal,
    max(greatest(from_ordinal, to_ordinal)) as to_ordinal
from legacy.post_answer
where dropped_because is null and ordered
group by post_id, ilk, field_id;

insert into post_field_option (post_id, field_option_id)
select span.post_id, option.id
from legacy.post_span span
join field_option option
    on option.field_id = span.field_id
    and option.ordinal = (span.from_ordinal + span.to_ordinal) / 2
where span.ilk = 'player';

insert into post_field_range (post_id, field_id, from_option_id, to_option_id)
select span.post_id, span.field_id, from_option.id, to_option.id
from legacy.post_span span
join field_option from_option
    on from_option.field_id = span.field_id and from_option.ordinal = span.from_ordinal
join field_option to_option
    on to_option.field_id = span.field_id and to_option.ordinal = span.to_ordinal
where span.ilk = 'group';
