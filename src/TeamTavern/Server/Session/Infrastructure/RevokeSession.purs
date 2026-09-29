module TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession) where

import Prelude

import Async (Async, foreach)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupToken)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Session.Domain.Token (hash)

queryString :: Query
queryString = Query """
    update session
    set revoked = true
    where session.token_hash = $1
    """

-- | Ends the session the cookies name, if they name one.
revokeSession :: ∀ querier errors. Querier querier =>
    querier -> Cookies -> Async (InternalTerror_ errors) Unit
revokeSession querier cookies =
    foreach (lookupToken cookies) \token -> do
        tokenHash <- hash token
        queryNone querier queryString (tokenHash : [])
