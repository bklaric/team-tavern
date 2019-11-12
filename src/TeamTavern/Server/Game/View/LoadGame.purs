module TeamTavern.Server.Game.View.LoadGame
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
import Postgres.Query (Query(..), QueryParameter, (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Game.Domain.Description (Description)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.Domain.Title (Title)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.Domain.Id (Id)

type LoadGameDto =
    { administratorId :: Int
    , handle :: String
    , title :: String
    , hasPlayerProfile :: Boolean
    , hasTeamProfile :: Boolean
    , fields :: Array
        { type :: Int
        , label :: String
        , key :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , option :: String
            })
        }
    }

type LoadGameResult =
    { administratorId :: Id
    , title :: Title
    , handle :: Handle
    , description :: Description
    , hasPlayerProfile :: Boolean
    , hasTeamProfile :: Boolean
    , fields :: Array
        { type :: Int
        , label :: String
        , key :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
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
        game.administrator_id as "administratorId",
        player_profile.id is not null as "hasPlayerProfile",
        team_profile.id is not null as "hasTeamProfile",
        coalesce(fields.fields, '[]') as fields
    from game
        left join fields on fields.game_id = game.id
        left join profile as player_profile on player_profile.game_id = game.id
            and player_profile.player_id = $2 and player_profile.type = 1
        left join profile as team_profile on team_profile.game_id = game.id
            and team_profile.player_id = $2 and team_profile.type = 2
    where game.handle = $1;
    """

queryParameters :: Handle -> Maybe CookieInfo -> Array QueryParameter
queryParameters handle auth = handle :| maybe 0 (_.id >>> unwrap) auth

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
    view @ { administratorId, handle, title, hasPlayerProfile, hasTeamProfile, fields } <-
        head games
        # Async.note (inj (SProxy :: SProxy "notFound") handle')
    pure
        { administratorId: wrap administratorId
        , title: wrap title
        , handle: wrap handle
        , description: wrap []
        , hasPlayerProfile
        , hasTeamProfile
        , fields
        }
