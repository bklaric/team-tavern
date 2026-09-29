module TeamTavern.Server.Session.End (end) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Either (Either(..))
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, removeCookieHeader)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Session.Infrastructure.RevokeSession (revokeSession)

-- | Signing out always succeeds for the browser that asked: its cookies are
-- | cleared even when revoking the session fails, which is only logged.
end :: ∀ left. Pool -> Cookies -> Async left _
end pool cookies = do
    result <- attempt $ revokeSession pool cookies
    case result of
        Left error -> fromEffect $ logError "Error revoking session" error
        Right _ -> pure unit
    pure $ noContent removeCookieHeader
