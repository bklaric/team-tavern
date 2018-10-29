module TeamTavern.Game.View.LoadGame where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Game.Domain.Description as Description
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Title as Name
import TeamTavern.Game.Domain.Types (View)
import TeamTavern.Player.Domain.PlayerId (toString)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Types (AuthInfo)

loadGameQuery :: Query
loadGameQuery = Query """
    select
        game.administrator_id as "administratorId",
        game.title,
        game.description,
        profile.id is not null as "hasProfile"
    from game
    left join profile on profile.game_id = game.id
        and profile.player_id = $2
    where game.handle = $1
    """

loadGameQueryParameters :: Handle -> Maybe AuthInfo -> Array QueryParameter
loadGameQueryParameters handle auth =
    [unwrap handle, maybe "0" (_.id >>> toString) auth] <#> QueryParameter

type GameViewModel =
    { administratorId :: Int
    , title :: String
    , description :: String
    , hasProfile :: Boolean
    }

type LoadGameError errors = Variant
    ( databaseError :: Error
    , unreadableView ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Handle
    , invalidView ::
        { handle :: Handle
        , view :: GameViewModel
        }
    | errors )

loadGame :: forall errors.
    Pool -> Handle -> Maybe AuthInfo -> Async (LoadGameError errors) View
loadGame pool handle auth = do
    result <- pool
        # query loadGameQuery (loadGameQueryParameters handle auth)
        # label (SProxy :: SProxy "databaseError")
    games <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableView") { result, errors: _ }
    view @ { administratorId, title, description, hasProfile } :: GameViewModel <-
        head games
        # Async.note (inj (SProxy :: SProxy "notFound") handle)
    { administratorId: _, title: _, handle, description: _, hasProfile }
        <$> PlayerId.create administratorId
        <*> Name.create'' title
        <*> Description.create'' description
        # Async.note (inj (SProxy :: SProxy "invalidView") { handle, view })
