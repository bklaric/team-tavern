module TeamTavern.Server.Team.View (Profile, Team, view) where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Maybe (Maybe)
import Data.Variant (match)
import Effect (Effect)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)
import TeamTavern.Server.Infrastructure.Log (logLoadSingleError)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst, teamAdjustedWeekdayFrom, teamAdjustedWeekdayTo, teamAdjustedWeekendFrom, teamAdjustedWeekendTo)

type RouteParams = { handle :: String, timezone :: String }

type Profile =
    { handle :: String
    , title :: String
    , fields :: Array
        { key :: String
        , label :: String
        , icon :: String
        , options :: Array
            { key :: String
            , label :: String
            }
        }
    , fieldValues :: Array
        { fieldKey :: String
        , optionKeys :: Array String
        }
    , newOrReturning :: Boolean
    , summary :: Array String
    , updated :: String
    , updatedSeconds :: Number
    }

type Team =
    { owner :: String
    , handle :: String
    , name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: Maybe String
    , timezone :: Maybe String
    , weekdayOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , weekendOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , about :: Array String
    , profiles :: Array Profile
    }

queryString :: String -> Query
queryString timezone = Query $ """
    select
        player.nickname as owner,
        team.handle,
        team.name,
        team.website,
        team.age_from as "ageFrom",
        team.age_to as "ageTo",
        team.locations,
        team.languages,
        team.has_microphone as "microphone",
        team.discord_server as "discordServer",
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
        team.about,
        -- '[]'::json as profiles
        coalesce(
            json_agg(
                json_build_object(
                    'handle', profile.handle,
                    'title', profile.title,
                    'fields', profile.fields,
                    'fieldValues', profile.field_values,
                    'newOrReturning', profile.new_or_returning,
                    'summary', profile.summary,
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
                coalesce(fields.fields, '[]') as fields,
                coalesce(field_values.field_values, '[]') as field_values,
                profile.new_or_returning,
                profile.summary,
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
                            )) filter (where profile_value.field_value_id is not null),
                            '[]'
                        ) as field_values
                    from (
                        select
                            field_value.team_profile_id,
                            field.key,
                            field.ilk,
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

loadTeam :: forall errors. Pool -> RouteParams -> Async (LoadSingleError errors) Team
loadTeam pool { handle, timezone } = queryFirst pool (queryString timezone) (handle : [])

logError :: LoadSingleError () -> Effect Unit
logError = logLoadSingleError "Error viewing team"

sendResponse :: Async (LoadSingleError ()) Team -> (forall left. Async left Response)
sendResponse = alwaysRight
    (match
        { internal: const internalServerError__
        , notFound: const notFound__
        }
    )
    (ok_ <<< writeJSON)

view :: forall left. Pool -> RouteParams -> Async left Response
view pool routeParams =
    sendResponse $ examineLeftWithEffect logError do
    loadTeam pool routeParams
