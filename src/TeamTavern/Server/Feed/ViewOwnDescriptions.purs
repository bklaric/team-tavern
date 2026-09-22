module TeamTavern.Server.Feed.ViewOwnDescriptions (ownDescriptionsQuery, viewOwnDescriptions) where

import Prelude

import Async (Async)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Routes.Feed.ViewOwnDescriptions as ViewOwnDescriptions
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- | A player's posts in a game, each as the description `Feed.sql` takes: its
-- | answers, and the account's country, age and languages for a player post.
-- | The hours are in the owner's timezone, as the post keeps them.
ownDescriptionsQuery :: Query
ownDescriptionsQuery = Query """
    select
        post.ilk as type,
        post.name,
        jsonb_build_object(
            'type', post.ilk,
            'options', coalesce((
                select jsonb_object_agg(field.key, options)
                from (
                    select option.field_id, jsonb_agg(option.key order by option.ordinal) as options
                    from post_field_option answer
                    join field_option option on option.id = answer.field_option_id
                    where answer.post_id = post.id
                    group by option.field_id
                ) answers
                join field on field.id = answers.field_id
            ), '{}'),
            'ranges', coalesce((
                select jsonb_object_agg(field.key, jsonb_build_object('from', range_from.key, 'to', range_to.key))
                from post_field_range range
                join field on field.id = range.field_id
                left join field_option range_from on range_from.id = range.from_option_id
                left join field_option range_to on range_to.id = range.to_option_id
                where range.post_id = post.id
            ), '{}'),
            'flags', to_jsonb(array(
                select field.key
                from post_field_flag flag
                join field on field.id = flag.field_id
                where flag.post_id = post.id
                order by field.ordinal
            )),
            'country', case when post.ilk = 'player' then owner.country end,
            'age', case when post.ilk = 'player'
                then date_part('year', age(now(), owner.birthday))::integer end,
            'regions', to_jsonb(post.regions),
            'ageFrom', post.age_from,
            'ageTo', post.age_to,
            'languages', to_jsonb(case when post.ilk = 'player' then owner.languages else post.languages end),
            'online', case when post.online_from is not null and post.online_to is not null
                then jsonb_build_object(
                    'from', to_char(post.online_from, 'HH24:MI'),
                    'to', to_char(post.online_to, 'HH24:MI'))
                end,
            'timezone', owner.timezone,
            'microphone', post.microphone
        ) as description
    from post
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    where game.handle = $1 and post.player_id = $2
    order by array_position(array['player', 'group', 'community'], post.ilk)
    """

viewOwnDescriptions :: ∀ left. Pool -> String -> Cookies -> Async left _
viewOwnDescriptions pool handle cookies =
    sendResponse "Error viewing own descriptions" do
    { id } <- ensureSignedIn pool cookies
    descriptions :: ViewOwnDescriptions.OkContent <- queryMany pool ownDescriptionsQuery (handle :| id)
    pure $ ok_ descriptions
