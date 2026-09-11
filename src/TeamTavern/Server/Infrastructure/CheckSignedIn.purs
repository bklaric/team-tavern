module TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import JavaScript.Npm.Pg.Result (rowCount)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo)

queryString :: Query
queryString = Query """
    select session.id
    from session
    join player on player.id = session.player_id
    where player.id = $1
        and lower(player.nickname) = lower($2)
        and session.token = $3
        and revoked = false
    """

checkSignedIn :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async errors (Maybe CookieInfo)
checkSignedIn querier cookies =
    case lookupCookieInfo cookies of
    Nothing -> pure Nothing
    Just cookieInfo @ { id, nickname, token } -> Async.unify do
        result
            <- querier
            #  query queryString (id : nickname :| token)
            #  lmap (const Nothing)
        if fromMaybe 0 (rowCount result) == 0
        then pure Nothing
        else pure $ Just cookieInfo
