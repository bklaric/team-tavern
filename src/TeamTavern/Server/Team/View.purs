module TeamTavern.Server.Team.View where

import Prelude

import Async (alwaysRight, examineLeftWithEffect, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label, labelMap)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (inj, match)
import Effect (foreachE)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (code)
import Perun.Response (internalServerError__, ok_)
import Postgres.Async.Query (query)
import Postgres.Error (constraint, detail, schema, severity, table)
import Postgres.Query (Query(..), (:), (:|))
import Postgres.Result (rows)
import Simple.JSON (writeJSON)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Log (logStamped, logt)
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Server.Player.UpdateDetails.ValidateTimezone (Timezone)
import TeamTavern.Server.Team.ViewByOwner (logLines, queryMany)
import Web.HTML.HTMLMediaElement.CanPlayType (CanPlayType(..))

type Team =
    { owner :: String
    , handle :: String
    , name :: String
    , website :: Maybe String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , discordServer :: Maybe String
    , timezone :: Maybe String
    , clientWeekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , clientWeekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , sourceWeekdayOnline :: Maybe
        { from :: String
        , to :: String
        }
    , sourceWeekendOnline :: Maybe
        { from :: String
        , to :: String
        }
    , about :: Array String
    }

prepareString :: String -> String
prepareString stringValue
    =  "'"
    <> (String.replace (String.Pattern "'") (String.Replacement "") stringValue)
    <> "'"

timezoneAdjustedTime :: String -> String -> String
timezoneAdjustedTime timezone timeColumn =
    """((current_date || ' ' || """ <> timeColumn <> """ || ' ' || team.timezone)::timestamptz
    at time zone """ <> prepareString timezone <> """)::time"""

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
        team.has_microphone as "hasMicrophone",
        team.discord_server as "discordServer",
        case when (player.nickname = $2) then team.timezone end as timezone,
        case
            when team.weekday_from is not null and team.weekday_to is not null
            then json_build_object(
                'from', to_char(""" <> timezoneAdjustedTime timezone "team.weekday_from" <> """, 'HH24:MI'),
                'to', to_char(""" <> timezoneAdjustedTime timezone "team.weekday_to" <> """, 'HH24:MI')
            )
        end as "clientWeekdayOnline",
        case
            when team.weekend_from is not null and team.weekend_to is not null
            then json_build_object(
                'from', to_char(""" <> timezoneAdjustedTime timezone "team.weekend_from" <> """, 'HH24:MI'),
                'to', to_char(""" <> timezoneAdjustedTime timezone "team.weekend_to" <> """, 'HH24:MI')
            )
        end as "clientWeekendOnline",
        case
            when (player.nickname = $2) and team.weekday_from is not null and team.weekday_to is not null
            then json_build_object(
                'from', to_char(team.weekday_from, 'HH24:MI'),
                'to', to_char(team.weekday_to, 'HH24:MI')
            )
        end as "sourceWeekdayOnline",
        case
            when (player.nickname = $2) and team.weekend_from is not null and team.weekend_to is not null
            then json_build_object(
                'from', to_char(team.weekend_from, 'HH24:MI'),
                'to', to_char(team.weekend_to, 'HH24:MI')
            )
        end as "sourceWeekendOnline",
        team.about
    from team
        join player on player.id = team.owner_id
    where lower(team.handle) = lower($1);
    """

queryFirst pool queryString parameters = do
    rows <- queryMany pool queryString parameters
    rows # head # note (inj (SProxy :: SProxy "internal")
        [ "Expected at least one row from database, got none." ])

loadTeam pool { handle, timezone } cookieInfo =
    queryFirst pool (queryString timezone) (handle :| (cookieInfo <#> _.nickname))

logError error = do
    logStamped "Error viewing team"
    error # match { internal: logLines, lol: logLines }

sendResponse = alwaysRight (const internalServerError__) (ok_ <<< writeJSON)

view pool cookies routeParams =
    sendResponse $ examineLeftWithEffect logError do
    cookieInfo <- checkSignedIn pool cookies
    team :: Team <- loadTeam pool routeParams cookieInfo
    pure team
