module TeamTavern.Server.Game.View.LoadGame
    (LoadGameResult, LoadGameError, loadGame) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.Domain.Title (Title)

type LoadGameDto =
    { handle :: String
    , title :: String
    , fields :: Array
        { type :: Int
        , label :: String
        , key :: String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        }
    }

type LoadGameResult =
    { title :: Title
    , handle :: Handle
    , fields :: Array
        { type :: Int
        , label :: String
        , key :: String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        }
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
        game.handle,
        game.title,
        coalesce(fields.fields, '[]') as fields
    from game
        left join fields on fields.game_id = game.id
    where game.handle = $1;
    """

loadGame
    :: forall errors
    .  Pool
    -> Handle
    -> Async (LoadGameError errors) LoadGameResult
loadGame pool handle' = do
    result <- pool
        # query queryString (handle' : [])
        # label (SProxy :: SProxy "databaseError")
    games :: Array LoadGameDto <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDto") { result, errors: _ }
    view @ { handle, title, fields } <-
        head games
        # Async.note (inj (SProxy :: SProxy "notFound") handle')
    pure
        { title: wrap title
        , handle: wrap handle
        , fields
        }
