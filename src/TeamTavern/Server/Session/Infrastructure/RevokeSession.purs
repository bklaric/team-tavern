module TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeOtherSessions, revokeSession) where

import Prelude

import Async (Async, foreach)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupToken)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Session.Domain.Token (Token, hash)

queryString :: Query
queryString = Query """
    update session
    set revoked = true
    where session.token_hash = $1
    """

othersQueryString :: Query
othersQueryString = Query """
    update session
    set revoked = true
    where player_id = $1 and token_hash <> $2
    """

-- | Ends the session the cookies name, if they name one.
revokeSession :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async (InternalTerror_ errors) Unit
revokeSession querier cookies =
    foreach (lookupToken cookies) \token -> do
        tokenHash <- hash token
        queryNone querier queryString (tokenHash : [])

-- | Ends every session of the player's but the one the token names, which
-- | changed how the account signs in. Whoever else held a session, perhaps the
-- | one the change is meant to lock out, has it ended with the old sign-in.
revokeOtherSessions :: ∀ querier errors. Querier querier =>
    querier -> Int -> Token -> Async (InternalTerror_ errors) Unit
revokeOtherSessions querier playerId token = do
    tokenHash <- hash token
    queryNone querier othersQueryString (playerId :| tokenHash)
