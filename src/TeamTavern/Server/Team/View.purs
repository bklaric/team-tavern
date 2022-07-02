module TeamTavern.Server.Team.View (view) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Variant (match)
import Effect (Effect)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import TeamTavern.Routes.Team.ViewTeam as ViewTeam
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)
import TeamTavern.Server.Infrastructure.Log (logLoadSingleError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, teamAdjustedWeekdayFrom, teamAdjustedWeekdayTo, teamAdjustedWeekendFrom, teamAdjustedWeekendTo)
import Yoga.JSON (writeJSON)

queryString :: String -> Query
queryString timezone = Query $ """
    select
        player.nickname as owner,
        team.handle,
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
        team.discord_tag as "discordTag",
        team.discord_server as "discordServer",
        team.steam_id as "steamId",
        team.riot_id as "riotId",
        team.battle_tag as "battleTag",
        team.ea_id as "eaId",
        team.psn_id as "psnId",
        team.gamer_tag as "gamerTag",
        team.friend_code as "friendCode",
        team.age_from as "ageFrom",
        team.age_to as "ageTo",
        team.locations,
        team.languages,
        team.microphone as "microphone",
        team.timezone,
        case
            when team.weekday_from is not null and team.weekday_to is not null
            then json_build_object(
                'clientFrom', to_char(""" <> teamAdjustedWeekdayFrom timezone <> """, 'HH24:MI'),
                'clientTo', to_char(""" <> teamAdjustedWeekdayTo timezone <> """, 'HH24:MI'),
                'sourceFrom', to_char(team.weekday_from, 'HH24:MI'),
                'sourceTo', to_char(team.weekday_to, 'HH24:MI')
            )
        end as "weekdayOnline",
        case
            when team.weekend_from is not null and team.weekend_to is not null
            then json_build_object(
                'clientFrom', to_char(""" <> teamAdjustedWeekendFrom timezone <> """, 'HH24:MI'),
                'clientTo', to_char(""" <> teamAdjustedWeekendTo timezone <> """, 'HH24:MI'),
                'sourceFrom', to_char(team.weekend_from, 'HH24:MI'),
                'sourceTo', to_char(team.weekend_to, 'HH24:MI')
            )
        end as "weekendOnline",
        coalesce(
            json_agg(
                json_build_object(
                    'handle', profile.handle,
                    'title', profile.title,
                    'allPlatforms', profile.all_platforms,
                    'size', profile.size,
                    'selectedPlatforms', profile.selected_platforms,
                    'fields', profile.fields,
                    'fieldValues', profile.field_values,
                    'newOrReturning', profile.new_or_returning,
                    'about', profile.about,
                    'ambitions', profile.ambitions,
                    'updated', profile.updated,
                    'updatedSeconds', profile.updated_seconds
                )
                order by profile.updated desc
            )
            filter (where profile.team_id is not null),
            '[]'
        ) as profiles
    from team
        join player on player.id = team.owner_id
        left join (
            select
                profile.team_id,
                game.handle,
                game.title,
                json_build_object(
                    'head', game.platforms[1],
                    'tail', game.platforms[2:]
                ) as all_platforms,
                profile.size,
                profile.platforms as selected_platforms,
                coalesce(fields.fields, '[]') as fields,
                coalesce(field_values.field_values, '[]') as field_values,
                profile.new_or_returning,
                profile.about,
                profile.ambitions,
                profile.updated::text,
                extract(epoch from (now() - updated)) as updated_seconds
            from team_profile as profile
                join game on game.id = profile.game_id
                left join (
                    select
                        game_id,
                        coalesce(
                            json_agg(
                                json_build_object(
                                    'ilk', ilk,
                                    'label', label,
                                    'key', key,
                                    'icon', icon,
                                    'options', options
                                )
                                order by ordinal
                            )
                            filter (where key is not null),
                            '[]'
                        )
                        as fields
                    from (
                        select
                            field.id,
                            field.game_id,
                            field.ilk,
                            field.label,
                            field.key,
                            field.icon,
                            field.ordinal,
                            json_agg(
                                json_build_object(
                                    'key', field_option.key,
                                    'label', field_option.label
                                )
                                order by field_option.ordinal
                            )
                            filter (where field_option.id is not null)
                            as options
                        from field
                            left join field_option on field_option.field_id = field.id
                        where
                            field.ilk = 2 or field.ilk = 3
                        group by
                            field.id
                        ) as field
                    group by game_id
                ) as fields on fields.game_id = game.id
                left join (
                    select
                        profile.id as profile_id,
                        coalesce(
                            json_agg(json_build_object(
                                'fieldKey', profile_value.key,
                                'optionKeys', multi
                            ) order by profile_value.ordinal
                            ) filter (where profile_value.field_value_id is not null),
                            '[]'
                        ) as field_values
                    from (
                        select
                            field_value.team_profile_id,
                            field.key,
                            field.ilk,
                            field.ordinal,
                            field_value.id as field_value_id,
                            json_agg(multi.key) as multi
                        from
                            team_profile_field_value as field_value
                        join field
                            on field.id = field_value.field_id
                        left join team_profile_field_value_option as field_value_option
                            on field_value_option.team_profile_field_value_id = field_value.id
                        left join field_option as multi
                            on multi.id = field_value_option.field_option_id
                        group by
                            field.id,
                            field_value.id
                    ) as profile_value
                    join team_profile as profile on profile.id = profile_value.team_profile_id
                    group by profile.id
                ) as field_values on field_values.profile_id = profile.id
        ) as profile on profile.team_id = team.id
    where lower(team.handle) = lower($1)
    group by team.id, player.nickname;
    """

loadTeam :: forall errors. Pool -> ViewTeam.RouteParams -> Async (LoadSingleError errors) ViewTeam.OkContent
loadTeam pool { handle, timezone } = queryFirstNotFound pool (queryString timezone) (handle : [])

logError :: LoadSingleError () -> Effect Unit
logError = logLoadSingleError "Error viewing team"

sendResponse :: Async (LoadSingleError ()) ViewTeam.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , notFound: const notFound__
        }
    )
    (ok_ <<< (writeJSON :: ViewTeam.OkContent -> String))

view :: forall left. Pool -> ViewTeam.RouteParams -> Async left Response
view pool routeParams =
    sendResponse $ examineLeftWithEffect logError do
    loadTeam pool routeParams
