-- What the import brought over and what it dropped, read from the tables
-- import.sql leaves in the legacy schema. The dropped answers, per game and
-- field, are how mapping.sql is reviewed.

\pset footer off

\echo
\echo 'Posts, active now (30 days, 90 for communities), and in their last week'
select game.handle as game, post.ilk, count(*) as posts,
    count(*) filter (where post.updated > now()
        - case when post.ilk = 'community' then interval '90 days' else interval '30 days' end) as active,
    count(*) filter (where post.updated > now()
            - case when post.ilk = 'community' then interval '90 days' else interval '30 days' end
        and post.updated <= now()
            - case when post.ilk = 'community' then interval '83 days' else interval '23 days' end) as last_week
from post
join game on game.id = post.game_id
group by game.handle, post.ilk
order by game.handle, post.ilk;

\echo 'Profiles dropped'
select old_game.handle as game, source.ilk, source.dropped_because, count(*) as profiles
from legacy.post_source source
join legacy.game old_game on old_game.id = source.old_game_id
where source.dropped_because is not null
group by old_game.handle, source.ilk, source.dropped_because
order by old_game.handle, source.ilk, source.dropped_because;

\echo 'Players, and how many carry a country'
select count(*) as players, count(country) as with_country, count(timezone) as with_timezone
from player;

\echo 'Player locations dropped, most common first: regions of the old tree, and countries the new list lacks'
select old.location, count(*) as players
from legacy.player old
join player on player.id = old.id
where old.location is not null and player.country is null
group by old.location
order by count(*) desc, old.location
limit 30;

\echo 'Group and community locations that name no region'
select location, count(*) as posts
from legacy.post_source source
cross join unnest(source.locations) location
where source.post_id is not null
    and not exists (select 1 from legacy.location_region_map map where map.old_name = location)
    and not exists (select 1 from legacy.location_country lc where lc.old_name = location)
group by location
order by count(*) desc, location;

\echo 'Answers dropped, per game and field'
select game.handle as game, answer.old_field, answer.dropped_because,
    sum(answer.n) as answers,
    string_agg(answer.old_option || ' ' || answer.n, ', ' order by answer.n desc, answer.old_option) as options
from (
    select game_id, old_field, old_option, dropped_because, count(*) as n
    from legacy.post_answer
    where dropped_because is not null
    group by game_id, old_field, old_option, dropped_because
) answer
join game on game.id = answer.game_id
group by game.handle, answer.old_field, answer.dropped_because
order by game.handle, answer.old_field, answer.dropped_because;
