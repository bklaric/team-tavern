module TeamTavern.Client.Shared.Me (fetchMe) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Variant (onMatch)
import TeamTavern.Client.Shared.Fetch (fetchSimple)
import TeamTavern.Routes.Player.ViewMe (ViewMe)
import TeamTavern.Routes.Player.ViewMe as ViewMe
import Type.Proxy (Proxy(..))

-- | The player signed in, or Nothing when nobody is or the server can't say.
-- | Only the server can tell: the session cookie is out of the page's reach.
fetchMe :: ∀ left. Async left (Maybe ViewMe.OkContent)
fetchMe = Async.attempt (fetchSimple (Proxy :: _ ViewMe)) <#> (hush >=> onMatch { ok: Just } (const Nothing))
