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
import Postgres.Query (Query(..), QueryParameter, (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.Domain.Title (Title)

type LoadGameDto =
    { handle :: String
    , title :: String
    , fields :: Array
        { ilk :: Int
        , label :: String
        , key :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    }

type LoadGameResult =
    { title :: Title
    , handle :: Handle
    , fields :: Array
        { ilk :: Int
        , label :: String
        , key :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
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
        coalesce(
            json_agg(
                json_build_object(
                    'ilk', field.ilk,
                    'label', field.label,
                    'key', field.key,
                    'icon', field.icon,
                    'required', field.required,
                    'domain', field.domain,
                    'options', field.options
                ) order by field.ordinal
            ) filter (where field.id is not null),
            '[]'
        ) as fields
    from game
        left join (
            select
                field.*,
                json_agg(
                    json_build_object(
                        'key', field_option.key,
                        'label', field_option.label
                    ) order by field_option.ordinal
                ) filter (where field_option.id is not null) as options
            from field
                left join field_option on field_option.field_id = field.id
            group by
                field.id
            ) as field on field.game_id = game.id
    where game.handle = $1
    group by game.id;
    """

queryParameters :: Handle -> Array QueryParameter
queryParameters handle = handle : []

loadGame :: forall errors. Pool -> Handle -> Async (LoadGameError errors) LoadGameResult
loadGame pool handle' = do
    result <- pool
        # query queryString (queryParameters handle')
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
