module TeamTavern.Server.Session.End (end) where

import Prelude

import Async (Async, attempt, foreach, fromEffect)
import Data.Either (Either(..))
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupCookieInfo, removeCookieHeader)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

queryString :: Query
queryString = Query """
    update session
    set revoked = true
    where player_id = $1 and token = $2
    """

revokeSession :: ∀ errors. Pool -> Cookies -> Async (InternalTerror_ errors) Unit
revokeSession pool cookies =
    foreach (lookupCookieInfo cookies) \{id, token} ->
        queryNone pool queryString (id :| token)

-- | Signing out always succeeds for the browser that asked: its cookies are
-- | cleared even when revoking the session fails, which is only logged.
end :: ∀ left. Pool -> Cookies -> Async left _
end pool cookies = do
    result <- attempt $ revokeSession pool cookies
    case result of
        Left error -> fromEffect $ logError "Error revoking session" error
        Right _ -> pure unit
    pure $ noContent removeCookieHeader
