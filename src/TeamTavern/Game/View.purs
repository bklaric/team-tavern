module TeamTavern.Game.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (inj)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import Postgres.Async.Query (query)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Game.View.LogError (logError) as View
import TeamTavern.Game.View.Response (response) as View
import TeamTavern.Game.View.Types (ViewError)
import TeamTavern.Player.Domain.PlayerId as PlayerId

validateHandle :: String -> Async ViewError Handle
validateHandle handle =
    handle
    # Handle.create'
    # labelMap (SProxy :: SProxy "handleInvalid") { handle, errors: _ }
    # Async.fromEither

type GameModel =
    { administratorId :: Int
    , name :: String
    , description :: String
    }

loadGameQuery :: Query
loadGameQuery = Query """
    select administrator_id as "administratorId", name, description
    from game
    where handle = $1
    """

loadGameQueryParameters :: Handle -> Array QueryParameter
loadGameQueryParameters handle = [unwrap handle] <#> QueryParameter

loadGame :: Pool -> Handle -> Async ViewError View
loadGame pool handle = do
    result <- pool
        # query loadGameQuery (loadGameQueryParameters handle)
        # label (SProxy :: SProxy "databaseError")
    games <- rows result
        # traverse read
        # label (SProxy :: SProxy "unreadableResult")
    { administratorId, name, description } :: GameModel <- head games
        # Async.note (inj (SProxy :: SProxy "notFound") handle)
    { administratorId: _, name: _, handle, description: _ }
        <$> PlayerId.create administratorId
        <*> Name.create'' name
        <*> Description.create'' description
        # Async.note (inj (SProxy :: SProxy "invalidView")
            { administratorId, name, handle, description })

handleView :: forall left. Pool -> String -> Async left Response
handleView pool handle' =
    View.response $ examineLeftWithEffect View.logError do
    -- Validate game handle from route.
    handle <- validateHandle handle'

    -- Load game from database.
    loadGame pool handle
