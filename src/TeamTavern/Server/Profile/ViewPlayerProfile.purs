module TeamTavern.Server.Profile.ViewPlayerProfile (viewPlayerProfile) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Routes.Profile.ViewPlayerProfile (RouteParams, OkContent)
import TeamTavern.Routes.Shared.Types (Timezone)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, adjustedWeekdayFrom, adjustedWeekdayTo, adjustedWeekendFrom, adjustedWeekendTo, queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Timezone -> Query
queryString timezone = Query $ """
    select profile.*
    from
        (select
            -- Base
            game.handle,
            game.title,
            -- Base
            player.nickname,
            -- Contacts
            player.discord_tag as "discordTag",
            player.steam_id as "steamId",
            player.riot_id as "riotId",
            player.battle_tag as "battleTag",
            player.ea_id as "eaId",
            player.ubisoft_username as "ubisoftUsername",
            player.psn_id as "psnId",
            player.gamer_tag as "gamerTag",
            player.friend_code as "friendCode",
            -- Details
            extract(year from age(player.birthday))::int as age,
            player.location,
            player.languages,
            player.microphone,
            case
                when player.weekday_from is not null and player.weekday_to is not null
                then json_build_object(
                    'from', to_char(""" <> adjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> adjustedWeekdayTo timezone <> """, 'HH24:MI')
                )
            end as "weekdayOnline",
            case
                when player.weekend_from is not null and player.weekend_to is not null
                then json_build_object(
                    'from', to_char(""" <> adjustedWeekendFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> adjustedWeekendTo timezone <> """, 'HH24:MI')
                )
            end as "weekendOnline",
            -- Profile
            json_build_object(
                'head', game.platforms[1],
                'tail', game.platforms[2:]
            ) as platforms,
            profile.platform as "platform",
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'ilk', field_values.ilk,
                            'key', field_values.key,
                            'label', field_values.label,
                            'icon', field_values.icon
                        ),
                        case
                            when field_values.ilk = 2 then 'option'
                            when field_values.ilk = 3 then 'options'
                        end,
                        case
                            when field_values.ilk = 2 then field_values.single
                            when field_values.ilk = 3 then field_values.multi
                        end
                    ) order by field_values.ordinal
                ) filter (where field_values.player_profile_id is not null),
                '[]'
            ) as "fieldValues",
            profile.about,
            profile.ambitions,
            profile.new_or_returning as "newOrReturning",
            profile.updated::text,
            extract(epoch from (now() - updated))::int as "updatedSeconds"
        from player_profile as profile
            join game on game.id = profile.game_id
            join player on player.id = profile.player_id
            left join (
                select
                    field_value.player_profile_id,
                    field.ilk,
                    field.label,
                    field.key,
                    field.icon,
                    field.ordinal,
                    jsonb_build_object(
                        'key', single.key,
                        'label', single.label
                    ) as single,
                    coalesce(
                        jsonb_agg(
                            jsonb_build_object(
                                'key', multi.key,
                                'label', multi.label
                            ) order by multi.ordinal
                        ) filter (where multi.label is not null),
                        '[]'
                    ) as multi
                from field
                    join player_profile_field_value as field_value on field_value.field_id = field.id
                    left join player_profile_field_value_option as field_value_option
                        on field_value_option.player_profile_field_value_id = field_value.id
                    left join field_option as single
                        on single.id = field_value_option.field_option_id
                        and field.ilk = 2
                    left join field_option as multi
                        on multi.id = field_value_option.field_option_id
                        and field.ilk = 3
                group by
                    field.id,
                    field_value.id,
                    single.id
            ) as field_values
                on field_values.player_profile_id = profile.id
        where lower(player.nickname) = lower($1) and game.handle = $2
        group by player.id, game.id, profile.id
        ) as profile
    order by profile.updated desc"""

selectPlayerProfile :: ∀ querier errors. Querier querier =>
    querier -> RouteParams -> Async (LoadSingleError errors) OkContent
selectPlayerProfile pool routeParams @ { nickname, handle, timezone } =
    queryFirstNotFound pool (queryString timezone) (nickname :| handle)
    # lmap (elaborate ("Error selecting player profile with route params: " <> show routeParams))

viewPlayerProfile :: ∀ left. Pool -> RouteParams -> Async left _
viewPlayerProfile pool routeParams =
    sendResponse  "Error viewing player profile" do
    ok_ <$> selectPlayerProfile pool routeParams
