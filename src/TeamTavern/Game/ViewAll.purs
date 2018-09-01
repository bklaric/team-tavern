module TeamTavern.Game.ViewAll where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (label)
import Data.List.Types (NonEmptyList)
import Data.Traversable (traverse)
import Data.Validated (Validated, hush)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant)
import Perun.Response (Response)
import Postgres.Async.Query (query_)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..))
import Postgres.Result (rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Game.ViewAll.LogError (logError) as ViewAll
import TeamTavern.Game.ViewAll.Response (response) as ViewAll
import TeamTavern.Game.ViewAll.Types (ViewAllError)
import TeamTavern.Player.Domain.PlayerId as PlayerId

loadGamesQuery :: Query
loadGamesQuery = Query """
    select administrator_id as "administratorId", name, handle, description
    from game
    """

type ViewModel =
    { administratorId :: Int
    , name :: String
    , handle :: String
    , description :: String
    }

validateViews
    :: forall errors
    .  Array ViewModel
    -> Async
        (Variant (invalidViews :: NonEmptyList ViewModel | errors))
        (Array View)
validateViews views= let
    validateView :: ViewModel -> Validated (NonEmptyList ViewModel) View
    validateView view @ { administratorId, name, handle, description } =
        { administratorId: _, name: _, handle: _, description: _ }
        <$> (PlayerId.create administratorId)
        <*> (Name.create name # hush)
        <*> (Handle.create handle # hush)
        <*> (Description.create description # hush)
        # Validated.note' view
    in
    views
    # traverse validateView
    # Async.fromValidated
    # label (SProxy :: SProxy "invalidViews")

loadGames :: Pool -> Async ViewAllError (Array View)
loadGames pool = do
    result <- pool
        # query_ loadGamesQuery
        # label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # label (SProxy :: SProxy "unreadableResult")
    validateViews views

handleViewAll :: forall left. Pool -> Async left Response
handleViewAll pool =
    ViewAll.response $ examineLeftWithEffect ViewAll.logError $
    -- Load games from database
    loadGames pool
