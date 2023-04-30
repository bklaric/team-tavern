module TeamTavern.Server.Profile.ViewTeamProfile (viewTeamProfile) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Routes.Profile.ViewTeamProfile (RouteParams, OkContent)
import TeamTavern.Routes.Shared.Types (Timezone)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound, teamAdjustedWeekdayFrom, teamAdjustedWeekdayTo, teamAdjustedWeekendFrom, teamAdjustedWeekendTo)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Timezone -> Query
queryString timezone = Query $ """
    select profile.*
    from
        (select
            -- Base
            game.handle as "gameHandle",
            game.title,
            -- Base
            team.handle,
            player.nickname as owner,
            -- Contacts
            team.discord_tag as "discordTag",
            team.discord_server as "discordServer",
            team.steam_id as "steamId",
            team.riot_id as "riotId",
            team.battle_tag as "battleTag",
            team.ea_id as "eaId",
            team.ubisoft_username as "ubisoftUsername",
            team.psn_id as "psnId",
            team.gamer_tag as "gamerTag",
            team.friend_code as "friendCode",
            -- Details
            case
                when team.organization = 'informal'
                then json_build_object(
                    'type', '"informal"'::jsonb,
                    'value', '{}'::jsonb
                )
                when team.organization = 'organized'
                then json_build_object(
                    'type', '"organized"'::jsonb,
                    'value', json_build_object(
                        'name', team.name,
                        'website', team.website
                    )
                )
            end as organization,
            team.age_from as "ageFrom",
            team.age_to as "ageTo",
            team.locations,
            team.languages,
            team.microphone,
            case
                when team.weekday_from is not null and team.weekday_to is not null
                then json_build_object(
                    'from', to_char(""" <> teamAdjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> teamAdjustedWeekdayTo timezone <> """, 'HH24:MI')
                )
            end as "weekdayOnline",
            case
                when team.weekend_from is not null and team.weekend_to is not null
                then json_build_object(
                    'from', to_char(""" <> teamAdjustedWeekendFrom timezone <> """, 'HH24:MI'),
                    'to', to_char(""" <> teamAdjustedWeekendTo timezone <> """, 'HH24:MI')
                )
            end as "weekendOnline",
            -- Profile
            profile.size,
            json_build_object(
                'head', game.platforms[1],
                'tail', game.platforms[2:]
            ) as "allPlatforms",
            profile.platforms as "selectedPlatforms",
            coalesce(
                jsonb_agg(
                    jsonb_build_object(
                        'field', jsonb_build_object(
                            'ilk', field_values.ilk,
                            'key', field_values.key,
                            'label', field_values.label,
                            'icon', field_values.icon
                        ),
                        'options', field_values.multi
                    ) order by field_values.ordinal
                ) filter (where field_values.team_profile_id is not null),
                '[]'
            ) as "fieldValues",
            profile.about,
            profile.ambitions,
            profile.new_or_returning as "newOrReturning",
            profile.updated::text,
            extract(epoch from (now() - profile.updated))::int as "updatedSeconds"
        from team_profile as profile
            join game on game.id = profile.game_id
            join team on team.id = profile.team_id
            join player on player.id = team.owner_id
            left join (
                select
                    field_value.team_profile_id,
                    field.ilk,
                    field.label,
                    field.key,
                    field.icon,
                    field.ordinal,
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
                    join team_profile_field_value as field_value on field_value.field_id = field.id
                    left join team_profile_field_value_option as field_value_option
                        on field_value_option.team_profile_field_value_id = field_value.id
                    left join field_option as multi
                        on multi.id = field_value_option.field_option_id
                group by
                    field.id,
                    field_value.id
            ) as field_values
                on field_values.team_profile_id = profile.id
        where lower(team.handle) = lower($1) and game.handle = $2
        group by team.id, player.nickname, game.id, profile.id
        ) as profile
    order by profile.updated desc"""

selectTeamProfile :: ∀ querier errors. Querier querier =>
    querier -> RouteParams -> Async (LoadSingleError errors) OkContent
selectTeamProfile pool routeParams @ { teamHandle, gameHandle, timezone } =
    queryFirstNotFound pool (queryString timezone) (teamHandle :| gameHandle)
    # lmap (elaborate ("Error selecting team profile with route params: " <> show routeParams))

viewTeamProfile :: ∀ left. Pool -> RouteParams -> Async left _
viewTeamProfile pool routeParams =
    sendResponse "Error viewing team profile" do
    ok_ <$> selectTeamProfile pool routeParams
