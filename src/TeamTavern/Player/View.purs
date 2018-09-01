module TeamTavern.Player.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Async (fromEither, note) as Async
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (inj)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (rows)
import Simple.JSON (read)
import Data.Bifunctor.Label (label) as Async
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Domain.About as About
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Types (NicknamedAbout)
import TeamTavern.Player.View.LogError (logError) as View
import TeamTavern.Player.View.Response (response) as View
import TeamTavern.Player.View.Types (ViewError)
import Validated as Validated

readNickname :: NonEmptyString -> Async ViewError Nickname
readNickname nickname =
    nickname
    # Nickname.fromNonEmpty'
    # lmap ({ nickname, errors: _} >>> inj (SProxy :: SProxy "nicknameInvalid"))
    # Async.fromEither

loadPlayerQuery :: Query
loadPlayerQuery = Query """
    select about
    from player
    where nickname = $1
    """

loadPlayerQueryParameters :: Nickname -> Array QueryParameter
loadPlayerQueryParameters nickname = [unwrap nickname] <#> QueryParameter

loadPlayer :: Pool -> Nickname -> Async ViewError NicknamedAbout
loadPlayer pool nickname = do
    result <- pool
        # query loadPlayerQuery (loadPlayerQueryParameters nickname)
        # Async.label (SProxy :: SProxy "databaseError")
    views <- rows result
        # traverse read
        # lmap (inj (SProxy :: SProxy "unreadableResult"))
        # Async.fromEither
    { about } :: { about :: String } <- head views
        # Async.note (inj (SProxy :: SProxy "notFound") nickname)
    { about: _, nickname }
        <$> (About.create about # Validated.hush)
        # Async.note (inj (SProxy :: SProxy "invalidView") { nickname, about })

handleView :: forall left. Pool -> NonEmptyString -> Async left Response
handleView pool nickname' =
    View.response $ examineLeftWithEffect View.logError do
    -- Read player nickname from route.
    nickname <- readNickname nickname'

    -- Load player from database.
    loadPlayer pool nickname
