module TeamTavern.Server.Player.ViewDetails.LoadDetails
    (LoadDetailsResult, loadDetails) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:|))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Profile.Routes (Timezone)

type LoadDetailsResult =
    { discordTag :: Maybe String
    , birthday :: Maybe String
    , age :: Maybe Int
    , languages :: Array String
    , country :: Maybe String
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
    , hasMicrophone :: Boolean
    }

type LoadDetailsError errors = Variant
    ( notFound :: String
    , unreadableAccount ::
        { account :: Foreign
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    | errors )

prepareString :: String -> String
prepareString stringValue
    =  "'"
    <> (String.replace (String.Pattern "'") (String.Replacement "") stringValue)
    <> "'"

timezoneAdjustedTime :: Timezone -> String -> String
timezoneAdjustedTime timezone timeColumn =
    """((current_date || ' ' || """ <> timeColumn <> """ || ' ' || player.timezone)::timestamptz
    at time zone """ <> prepareString timezone <> """)::time"""

queryString :: Timezone -> Query
queryString timezone = Query $ """
    select
        player.discord_tag as "discordTag",
        case when $2 then to_char(player.birthday, 'yyyy-mm-dd') end as birthday,
        extract(year from age(player.birthday))::int as age,
        player.languages,
        player.country,
        case when $2 then player.timezone end as timezone,
        case
            when player.weekday_from is not null and player.weekday_to is not null
            then json_build_object(
                'from', to_char(""" <> timezoneAdjustedTime timezone "player.weekday_from" <> """, 'HH24:MI'),
                'to', to_char(""" <> timezoneAdjustedTime timezone "player.weekday_to" <> """, 'HH24:MI')
            )
        end as "clientWeekdayOnline",
        case
            when player.weekend_from is not null and player.weekend_to is not null
            then json_build_object(
                'from', to_char(""" <> timezoneAdjustedTime timezone "player.weekend_from" <> """, 'HH24:MI'),
                'to', to_char(""" <> timezoneAdjustedTime timezone "player.weekend_to" <> """, 'HH24:MI')
            )
        end as "clientWeekendOnline",
        case
            when $2 and player.weekday_from is not null and player.weekday_to is not null
            then json_build_object(
                'from', to_char(player.weekday_from, 'HH24:MI'),
                'to', to_char(player.weekday_to, 'HH24:MI')
            )
        end as "sourceWeekdayOnline",
        case
            when $2 and player.weekend_from is not null and player.weekend_to is not null
            then json_build_object(
                'from', to_char(player.weekend_from, 'HH24:MI'),
                'to', to_char(player.weekend_to, 'HH24:MI')
            )
        end as "sourceWeekendOnline",
        player.has_microphone as "hasMicrophone"
    from player
    where lower(player.nickname) = lower($1)
    """

loadDetails
    :: forall errors
    .  Pool
    -> String
    -> Timezone
    -> Maybe CookieInfo
    -> Async (LoadDetailsError errors) LoadDetailsResult
loadDetails pool nickname timezone cookieInfo = do
    let samePlayer =
            maybe false ((_.nickname) >>> unwrap >>> (_ == nickname)) cookieInfo
    result
        <- pool
        #  query (queryString timezone) (nickname :| samePlayer)
        #  label (SProxy :: SProxy "databaseError")
    account
        <- rows result
        #  Array.head
        #  Async.note (inj (SProxy :: SProxy "notFound") nickname)
    account'
        <- read account
        #  labelMap (SProxy :: SProxy "unreadableAccount") { account, errors: _ }
    pure account'
