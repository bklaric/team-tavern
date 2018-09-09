module TeamTavern.Game.ViewAll.LoadGames
    (GameViewModel, LoadGamesError, loadGames) where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (label, labelMap)
import Data.List.Types (NonEmptyList)
import Data.Traversable (traverse)
import Data.Validated (Validated, hush)
import Data.Validated as Validated
import Data.Variant (SProxy(..), Variant)
import Foreign (ForeignError)
import Postgres.Async.Query (query_)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Player.Domain.PlayerId as PlayerId

loadGamesQuery :: Query
loadGamesQuery = Query """
    select administrator_id as "administratorId", name, handle, description
    from game
    order by created desc
    """

type GameViewModel =
    { administratorId :: Int
    , name :: String
    , handle :: String
    , description :: String
    }

type LoadGamesError errors = Variant
    ( unreadableViews ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , invalidViews :: NonEmptyList GameViewModel
    , databaseError :: Error
    | errors )

validateViews
    :: forall errors
    .  Array GameViewModel
    -> Async
        (Variant (invalidViews :: NonEmptyList GameViewModel | errors))
        (Array View)
validateViews views = let
    validateView ::
        GameViewModel -> Validated (NonEmptyList GameViewModel) View
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

loadGames :: forall errors.
    Pool -> Async (LoadGamesError errors) (Array View)
loadGames pool = do
    result <- pool
        # query_ loadGamesQuery
        # label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableViews") { result, errors: _ }
    validateViews views
