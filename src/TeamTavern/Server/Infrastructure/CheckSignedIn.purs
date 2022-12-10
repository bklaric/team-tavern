module TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn, checkSignedIn') where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Postgres.Async.Query (query)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import Postgres.Result (rowCount)
import Tasync (Tasync, getCookies)
import Tasync as Tasync
import TeamTavern.Server.Infrastructure.Cookie (Cookies, CookieInfo, lookupCookieInfo)

queryString ∷ Query
queryString = Query """
    select session.id
    from session
    join player on player.id = session.player_id
    where player.id = $1
        and player.nickname = $2
        and session.token = $3
        and revoked = false
    """

checkSignedIn ∷ ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async errors (Maybe CookieInfo)
checkSignedIn querier cookies =
    case lookupCookieInfo cookies of
    Nothing -> pure Nothing
    Just cookieInfo @ { id, nickname, token } -> Async.unify do
        result
            <- querier
            #  query queryString (id : nickname :| token)
            #  lmap (const Nothing)
        if rowCount result == 0
        then pure Nothing
        else pure $ Just cookieInfo

-- checkSignedIn' ∷ ∀ p q b l. Tasync p q b l (Maybe CookieInfo)
-- checkSignedIn' = do
--     cookies <- getCookies
--     case lookupCookieInfo cookies of
--         Nothing -> pure Nothing
--         Just cookieInfo @ { id, nickname, token } -> Tasync.unify do
--             result
--                 <- Tasync.query queryString (id : nickname :| token)
--                 #  lmap (const Nothing)
--             if rowCount result == 0
--             then pure Nothing
--             else pure $ Just cookieInfo
