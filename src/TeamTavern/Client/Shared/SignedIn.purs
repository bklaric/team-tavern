module TeamTavern.Client.Shared.SignedIn (signedIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (Either(..))
import Data.Variant (onMatch)
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Routes.Player.ViewMe (ViewMe)
import Type.Proxy (Proxy(..))

-- | Whether the server takes the session in the cookies. The cookies alone
-- | outlive a session the server has dropped, and the server removes them only
-- | when it is asked.
signedIn :: ∀ left. Async left Boolean
signedIn = do
    cookie <- hasPlayerIdCookie
    if not cookie then pure false else
        Async.attempt (fetchSimple (Proxy :: _ ViewMe)) <#> case _ of
            Right response -> response # onMatch { ok: const true } (const false)
            Left _ -> false
