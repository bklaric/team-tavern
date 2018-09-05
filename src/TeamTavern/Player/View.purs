module TeamTavern.Player.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async (fromEffect, fromEither, note) as Async
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Bifunctor.Label (label) as Async
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Validated as Validated
import Data.Variant (inj)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Perun.Response (Response)
import Postgres.Async.Query (query)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rows)
import Simple.JSON (read)
import TeamTavern.Game.Domain.Handle as Handle
import TeamTavern.Game.Domain.Name as Name
import TeamTavern.Player.Domain.About as About
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Types (View)
import TeamTavern.Player.View.LogError (logError) as View
import TeamTavern.Player.View.Response (response) as View
import TeamTavern.Player.View.Types (ViewError)
import TeamTavern.Profile.Domain.Summary as Summary

readNickname :: NonEmptyString -> Async ViewError Nickname
readNickname nickname =
    nickname
    # Nickname.fromNonEmpty'
    # lmap ({ nickname, errors: _} >>> inj (SProxy :: SProxy "nicknameInvalid"))
    # Async.fromEither

loadPlayerQuery :: Query
loadPlayerQuery = Query """
    select
        player.about,
        json_agg(json_build_object(
            'handle', game.handle,
            'name', game.name,
            'summary', profile.summary
        )) as profiles
    from profile
    join player on player.id = profile.player_id
    join game on game.id = profile.game_id
    where player.nickname = $1
    group by player.about
    """

loadPlayerQueryParameters :: Nickname -> Array QueryParameter
loadPlayerQueryParameters nickname = [unwrap nickname] <#> QueryParameter

type PlayerViewModel =
    { about :: String
    , profiles :: Array
        { handle :: String
        , name :: String
        , summary :: String
        }
    }

loadPlayer :: Pool -> Nickname -> Async ViewError View
loadPlayer pool nickname = do
    result <- pool
        # query loadPlayerQuery (loadPlayerQueryParameters nickname)
        # Async.label (SProxy :: SProxy "databaseError")
    Async.fromEffect $ log $ unsafeStringify $ rows result
    views <- rows result
        # traverse read
        # lmap (inj (SProxy :: SProxy "unreadableResult"))
        # Async.fromEither
    view @ { about, profiles } :: PlayerViewModel <- head views
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    { nickname, about: _, profiles: _ }
        <$> (About.create about # Validated.hush)
        <*> (profiles # traverse \{ handle, name, summary } ->
            { handle: _, name: _, summary: _ }
            <$> (Handle.create handle # Validated.hush)
            <*> (Name.create name # Validated.hush)
            <*> (Summary.create summary # Validated.hush))
        # Async.note (inj (SProxy :: SProxy "invalidView") { nickname, view })

handleView :: forall left. Pool -> NonEmptyString -> Async left Response
handleView pool nickname' =
    View.response $ examineLeftWithEffect View.logError do
    -- Read player nickname from route.
    nickname <- readNickname nickname'

    -- Load player from database.
    loadPlayer pool nickname
