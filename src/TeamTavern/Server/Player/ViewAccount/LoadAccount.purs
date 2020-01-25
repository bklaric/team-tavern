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
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), (:))
import Postgres.Result (rows)
import Simple.JSON.Async (read)

type LoadAccountResult =
    { id :: Int
    , nickname :: String
    , discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , about :: Array String
    , notify :: Boolean
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
        player.id,
        player.nickname,
        player.discord_tag as "discordTag",
        to_char(player.birthday, 'yyyy-mm-dd') as birthday,
        player.languages,
        player.has_microphone as "hasMicrophone",
        player.about,
        player.notify
    from player
    where lower(player.nickname) = lower($1)
    """

loadAccount
    :: forall errors
    .  Client
    -> String
    -> Async (LoadAccountError errors) LoadAccountResult
loadAccount client nickname = do
    result <- client
        # query queryString (nickname : [])
        # label (SProxy :: SProxy "databaseError")
    account <- rows result
        # Array.head
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    account' :: LoadAccountResult <- read account
        # labelMap (SProxy :: SProxy "unreadableAccount") { account, errors: _ }
    pure account'
