module TeamTavern.Server.Session.End where

import Prelude

import Async (Async, right)
import Data.Variant (inj)
import TeamTavern.Server.Infrastructure.Cookie (removeCookieHeader)
import Type.Proxy (Proxy(..))

end :: forall left. Async left _
end = right $ inj (Proxy :: _ "noContent") { headers: removeCookieHeader }
