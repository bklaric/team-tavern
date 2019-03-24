module TeamTavern.Game.View.LoadGame
    (LoadGameResult, LoadGameError, loadGame) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
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
import TeamTavern.Game.Domain.Description (Description)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Title (Title)
import TeamTavern.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Player.Domain.Id (Id)

type LoadGameDto =
    { administratorId :: Int
    , handle :: String
    , title :: String
    , description :: Array String
    , hasProfile :: Boolean
    }

type LoadGameResult =
    { administratorId :: Id
    , title :: Title
    , handle :: Handle
    , description :: Description
    , hasProfile :: Boolean
    }

type LoadGameError errors = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Handle
    | errors )

queryString :: Query
queryString = Query """
    select
        game.administrator_id as "administratorId",
        game.handle,
        game.title,
        game.description,
        profile.id is not null as "hasProfile"
    from game
    left join profile on profile.game_id = game.id
        and profile.player_id = $2
    where game.handle = $1
    """

queryParameters :: Handle -> Maybe CookieInfo -> Array QueryParameter
queryParameters handle auth = handle : maybe 0 (_.id >>> unwrap) auth : []

loadGame
    :: forall errors
    .  Pool
    -> Handle
    -> Maybe CookieInfo
    -> Async (LoadGameError errors) LoadGameResult
loadGame pool handle' auth = do
    result <- pool
        # query queryString (queryParameters handle' auth)
        # label (SProxy :: SProxy "databaseError")
    games :: Array LoadGameDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDto") { result, errors: _ }
    view @ { administratorId, handle, title, description, hasProfile } <-
        head games
        # Async.note (inj (SProxy :: SProxy "notFound") handle')
    pure
        { administratorId: wrap administratorId
        , title: wrap title
        , handle: wrap handle
        , description: description <#> wrap # wrap
        , hasProfile
        }
