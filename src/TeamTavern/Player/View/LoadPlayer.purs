module TeamTavern.Player.View.LoadPlayer (LoadPlayerResult, loadPlayer) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label) as Async
import Data.Bifunctor.Label (labelMap)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter, (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Player.Domain.About (About)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Player.Domain.Nickname (Nickname)

type LoadPlayerDto =
    { id :: Int
    , nickname :: String
    , about :: Array String
    }

type LoadPlayerResult =
    { id :: Id
    , nickname :: Nickname
    , about :: About
    }

type LoadPlayerError errors = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Nickname
    | errors )

queryString :: Query
queryString = Query """
    select player.id, player.nickname, player.about
    from player
    where player.nickname = $1
    """

queryParameters :: Nickname -> Array QueryParameter
queryParameters nickname = nickname : []

loadPlayer :: forall errors.
    Pool -> Nickname -> Async (LoadPlayerError errors) LoadPlayerResult
loadPlayer pool nickname' = do
    result <- pool
        # query queryString (queryParameters nickname')
        # Async.label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDto") { result, errors: _ }
    view @ { id, nickname, about } :: LoadPlayerDto <- head views
        # Async.note (inj (SProxy :: SProxy "notFound") nickname')
    pure
        { id: wrap id
        , nickname: wrap nickname
        , about: about <#> wrap # wrap
        }
