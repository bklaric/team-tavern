module TeamTavern.Server.Team.View (Team, view) where

import Prelude

import Async (alwaysRight, examineLeftWithEffect)
import Data.Maybe (Maybe)
import Perun.Response (internalServerError__, ok_)
import Postgres.Query (Query(..), (:|))
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Infrastructure.Postgres (internalHandler, queryFirst, teamAdjustedWeekdayFrom, teamAdjustedWeekdayTo, teamAdjustedWeekendFrom, teamAdjustedWeekendTo)

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
        case when (player.nickname = $2) then team.timezone end as timezone,
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
        team.about
    from team
        join player on player.id = team.owner_id
    where lower(team.handle) = lower($1);
    """

loadTeam pool { handle, timezone } cookieInfo =
    queryFirst pool (queryString timezone) (handle :| (cookieInfo <#> _.nickname))

logError = Log.logError "Error viewing team" internalHandler

sendResponse = alwaysRight (const internalServerError__) (ok_ <<< writeJSON)

view pool cookies routeParams =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- checkSignedIn pool cookies
    team :: Team <- loadTeam pool routeParams cookieInfo
    pure team
