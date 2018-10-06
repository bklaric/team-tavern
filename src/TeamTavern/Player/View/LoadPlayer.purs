module TeamTavern.Player.View.LoadPlayer
    (PlayerViewModel, LoadPlayerError, loadPlayer) where

import Prelude

import Async (Async)
import Async (note) as Async
import Data.Array (head)
import Data.Bifunctor.Label (label) as Async
import Data.Bifunctor.Label (labelMap)
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
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Title as Name
import TeamTavern.Player.Domain.About as About
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Types (View)
import TeamTavern.Profile.Domain.Summary as Summary

loadPlayerQuery :: Query
loadPlayerQuery = Query """
    select
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

type PlayerViewModel =
    { about :: String
    , profiles :: Array
        { handle :: String
        , title :: String
        , summary :: String
        }
    }

type LoadPlayerError errors = Variant
    ( databaseError :: Error
    , unreadableResult ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Nickname
    , invalidView ::
        { nickname :: Nickname
        , view :: PlayerViewModel
        }
    | errors )

loadPlayer :: forall errors.
    Pool -> Nickname -> Async (LoadPlayerError errors) View
loadPlayer pool nickname = do
    result <- pool
        # query loadPlayerQuery (loadPlayerQueryParameters nickname)
        # Async.label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableResult") { result, errors: _ }
    view @ { about, profiles } :: PlayerViewModel <- head views
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    { nickname, about: _, profiles: _ }
        <$> About.create'' about
        <*> (profiles # traverse \{ handle, title, summary } ->
            { handle: _, title: _, summary: _ }
            <$> Handle.create'' handle
            <*> Name.create'' title
            <*> Summary.create'' summary)
        # Async.note (inj (SProxy :: SProxy "invalidView") { nickname, view })
