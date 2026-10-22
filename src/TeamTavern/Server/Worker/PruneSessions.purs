module TeamTavern.Server.Worker.PruneSessions (pruneSessions) where

import Prelude

import Async (Async)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Session.Domain.Token (sessionDays)

queryString :: Query
queryString = Query """
    delete from session
    where revoked or last_used <= current_timestamp - make_interval(days => $1)
    """

-- | Deletes the sessions that have ended or lapsed, which `checkSignedIn`
-- | refuses anyway.
pruneSessions :: ∀ errors. Pool -> Async (InternalTerror_ errors) Unit
pruneSessions pool = queryNone pool queryString (sessionDays : [])
