module TeamTavern.Player.View.LoadPlayer (LoadPlayerResult, loadPlayer) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label) as Async
import Data.Bifunctor.Label (labelMap)
import Data.Newtype (unwrap, wrap)
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
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Title (Title)
import TeamTavern.Player.Domain.About (About)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Profile.Domain.Summary (Summary)

type LoadPlayerDto =
    { id :: Int
    , nickname :: String
    , about :: String
    , profiles :: Array
        { handle :: String
        , title :: String
        , summary :: String
        }
    }

type LoadPlayerResult =
    { id :: Id
    , nickname :: Nickname
    , about :: About
    , profiles :: Array
        { handle :: Handle
        , title :: Title
        , summary :: Summary
        }
    }

type LoadPlayerError errors = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Nickname
    | errors )

loadPlayerQuery :: Query
loadPlayerQuery = Query """
    select
        player.id,
        player.nickname,
        player.about,
        coalesce(json_agg(json_build_object(
            'handle', game.handle,
            'title', game.title,
            'summary', profile.summary
        ) order by profile.created desc)
        filter (where game.handle is not null), '[]'::json) as profiles
    from profile
    right join player on player.id = profile.player_id
    left join game on game.id = profile.game_id
    where player.nickname = $1
    group by player.about
    """

loadPlayerQueryParameters :: Nickname -> Array QueryParameter
loadPlayerQueryParameters nickname = [unwrap nickname] <#> QueryParameter

loadPlayer :: forall errors.
    Pool -> Nickname -> Async (LoadPlayerError errors) LoadPlayerResult
loadPlayer pool nickname' = do
    result <- pool
        # query loadPlayerQuery (loadPlayerQueryParameters nickname')
        # Async.label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableDto") { result, errors: _ }
    view @ { id, nickname, about, profiles } :: LoadPlayerDto <- head views
        # Async.note (inj (SProxy :: SProxy "notFound") nickname')
    pure
        { id: wrap id
        , nickname: wrap nickname
        , about: wrap about
        , profiles: profiles <#> \{ handle, title, summary } ->
            { handle: wrap handle
            , title: wrap title
            , summary: wrap summary}
        }
