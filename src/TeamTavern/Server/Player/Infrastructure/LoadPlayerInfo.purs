module TeamTavern.Server.Player.Infrastructure.LoadPlayerInfo
    (LoadPlayerInfoResult, LoadPlayerInfoError, loadPlayerInfo) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label) as Async
import Data.Bifunctor.Label (labelMap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)

type LoadPlayerInfoResult =
    { id :: Int
    , nickname :: String
    , email :: String
    , notify :: Boolean
    }

type LoadPlayerInfoError errors = Variant
    ( databaseError :: Error
    , unreadableResult ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: String
    | errors )

queryString :: Query
queryString = Query """
    select player.id, player.nickname, player.email, player.notify
    from player
    where lower(player.nickname) = lower($1)
    """

loadPlayerInfo :: forall querier errors. Querier querier =>
    querier -> String -> Async (LoadPlayerInfoError errors) LoadPlayerInfoResult
loadPlayerInfo querier nickname' = do
    result <- querier
        # query queryString (nickname' : [])
        # Async.label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableResult") { result, errors: _ }
    view :: LoadPlayerInfoResult <- head views
        # Async.note (inj (SProxy :: SProxy "notFound") nickname')
    pure view
