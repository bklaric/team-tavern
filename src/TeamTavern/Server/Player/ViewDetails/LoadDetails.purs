module TeamTavern.Server.Player.ViewDetails.LoadDetails
    (LoadDetailsResult, loadDetails) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Foreign (Foreign, MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (rows)
import Simple.JSON.Async (read)

type LoadDetailsResult =
    { discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
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

queryString :: Query
queryString = Query """
    select
        player.discord_tag as "discordTag",
        to_char(player.birthday, 'yyyy-mm-dd') as birthday,
        player.languages,
        player.country,
        player.timezone,
        to_char(player.weekday_from, 'HH24:MI') as "weekdayFrom",
        to_char(player.weekday_to, 'HH24:MI') as "weekdayTo",
        to_char(player.weekend_from, 'HH24:MI') as "weekendFrom",
        to_char(player.weekend_to, 'HH24:MI') as "weekendTo",
        player.has_microphone as "hasMicrophone"
    from player
    where lower(player.nickname) = lower($1)
    """

loadDetails
    :: forall errors
    .  Pool
    -> String
    -> Async (LoadDetailsError errors) LoadDetailsResult
loadDetails pool nickname = do
    result <- pool
        # query queryString (nickname : [])
        # label (SProxy :: SProxy "databaseError")
    account <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    account' :: LoadDetailsResult <- read account
        # labelMap (SProxy :: SProxy "unreadableAccount") { account, errors: _ }
    pure account'
