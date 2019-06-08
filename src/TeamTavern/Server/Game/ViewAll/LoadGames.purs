module TeamTavern.Server.Game.ViewAll.LoadGames
    (LoadGamesResult, LoadGamesError, loadGames) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query_)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Game.Domain.Description (Description)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.Domain.Title (Title)
import TeamTavern.Server.Player.Domain.Id (Id)

type LoadGamesDto =
    { administratorId :: Int
    , title :: String
    , handle :: String
    , description :: Array String
    , profileCount :: Int
    }

type LoadGamesResult =
    { administratorId :: Id
    , title :: Title
    , handle :: Handle
    , description :: Description
    , profileCount :: Int
    }

type LoadGamesError errors = Variant
    ( unreadableDtos ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    | errors
    )

loadGamesQuery :: Query
loadGamesQuery = Query """
    select
        game.administrator_id as "administratorId",
        game.title,
        game.handle,
        game.description,
        count(profile.id)::integer as "profileCount"
    from game
        left join profile on profile.game_id = game.id
    group by
        game.administrator_id,
        game.title,
        game.handle,
        game.description,
        game.created
    order by game.created desc
    """

loadGames :: forall errors.
    Pool -> Async (LoadGamesError errors) (Array LoadGamesResult)
loadGames pool = do
    result <- pool
        # query_ loadGamesQuery
        # label (SProxy :: SProxy "databaseError")
    views :: Array LoadGamesDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDtos") { result, errors: _ }
    pure $ views <#>
        \{ administratorId, title, handle, description, profileCount } ->
            { administratorId: wrap administratorId
            , title: wrap title
            , handle: wrap handle
            , description: description <#> wrap # wrap
            , profileCount
            }
