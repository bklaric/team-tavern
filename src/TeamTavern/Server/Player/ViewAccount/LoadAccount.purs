module TeamTavern.Server.Player.ViewAccount.LoadAccount where

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

type LoadAccountResult =
    { discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , weekdayStart :: Maybe String
    , weekdayEnd :: Maybe String
    , weekendStart :: Maybe String
    , weekendEnd :: Maybe String
    , hasMicrophone :: Boolean
    }

type LoadAccountError errors = Variant
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
        to_char(player.weekday_start, 'HH24:MI') as "weekdayStart",
        to_char(player.weekday_end, 'HH24:MI') as "weekdayEnd",
        to_char(player.weekend_start, 'HH24:MI') as "weekendStart",
        to_char(player.weekend_end, 'HH24:MI') as "weekendEnd",
        player.has_microphone as "hasMicrophone"
    from player
    where lower(player.nickname) = lower($1)
    """

loadAccount
    :: forall errors
    .  Pool
    -> String
    -> Async (LoadAccountError errors) LoadAccountResult
loadAccount pool nickname = do
    result <- pool
        # query queryString (nickname : [])
        # label (SProxy :: SProxy "databaseError")
    account <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    account' :: LoadAccountResult <- read account
        # labelMap (SProxy :: SProxy "unreadableAccount") { account, errors: _ }
    pure account'
