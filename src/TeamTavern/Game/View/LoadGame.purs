module TeamTavern.Game.View.LoadGame where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (ForeignError)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Player.Domain.PlayerId as PlayerId

loadGameQuery :: Query
loadGameQuery = Query """
    select administrator_id as "administratorId", name, description
    from game
    where handle = $1
    order by created desc
    """

loadGameQueryParameters :: Handle -> Array QueryParameter
loadGameQueryParameters handle = [unwrap handle] <#> QueryParameter

type GameViewModel =
    { administratorId :: Int
    , name :: String
    , description :: String
    }

type LoadGameError errors = Variant
    ( databaseError :: Error
    , unreadableView ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , notFound :: Handle
    , invalidView ::
        { handle :: Handle
        , view :: GameViewModel
        }
    | errors )

loadGame :: forall errors. Pool -> Handle -> Async (LoadGameError errors) View
loadGame pool handle = do
    result <- pool
        # query loadGameQuery (loadGameQueryParameters handle)
        # label (SProxy :: SProxy "databaseError")
    games <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableView") { result, errors: _ }
    view @ { administratorId, name, description } :: GameViewModel <-
        head games
        # Async.note (inj (SProxy :: SProxy "notFound") handle)
    { administratorId: _, name: _, handle, description: _ }
        <$> PlayerId.create administratorId
        <*> Name.create'' name
        <*> Description.create'' description
        # Async.note (inj (SProxy :: SProxy "invalidView") { handle, view })
