module TeamTavern.Server.Session.End where

import Prelude

import Async (Async, right)
import Jarilo (noContent)
import TeamTavern.Server.Infrastructure.Cookie (removeCookieHeader)

end :: ∀ left. Async left _
end = right $ noContent removeCookieHeader
