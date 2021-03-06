module TeamTavern.Server.Session.End where

import Prelude

import Async (Async, right)
import Perun.Response (Response, noContent)
import TeamTavern.Server.Infrastructure.Cookie (removeCookieHeader)

end :: forall void. Async void Response
end = right $ noContent removeCookieHeader
