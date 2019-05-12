module TeamTavern.Server.Player.ViewHeader.LoadHeader
    (LoadHeaderResult, loadHeader) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
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
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.Domain.Nickname (Nickname(..))

type LoadHeaderResult = { nickname :: Nickname }

type LoadHeaderError errors = Variant
    ( databaseError :: Error
    , unreadableHeader ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Id
    | errors )

queryString :: Query
queryString = Query """
    select nickname
    from player
    where id = $1
    """

queryParameters :: Id -> Array QueryParameter
queryParameters id = id : []

loadHeader :: forall errors.
    Pool -> Id -> Async (LoadHeaderError errors) LoadHeaderResult
loadHeader pool id = do
    result <- pool
        # query queryString (queryParameters id)
        # label (SProxy :: SProxy "databaseError")
    headers <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableHeader") { result, errors: _ }
    { nickname } :: { nickname :: String } <- head headers
        # note (inj (SProxy :: SProxy "notFound") id)
    pure $ { nickname: Nickname nickname }
