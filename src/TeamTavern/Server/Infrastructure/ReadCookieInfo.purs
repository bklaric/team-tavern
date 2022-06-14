module TeamTavern.Server.Infrastructure.ReadCookieInfo where

import Prelude

import Async (Async)
import Async as Async
import Data.Map (Map)
import Data.Variant (Variant, inj)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, lookupCookieInfo)
import Type.Proxy (Proxy(..))

type ReadCookieInfoError error = Variant
    ( cookieInfoNotPresent :: Map String String
    | error )

readCookieInfo :: forall errors.
    Map String String -> Async (ReadCookieInfoError errors) CookieInfo
readCookieInfo cookies = lookupCookieInfo cookies
    # Async.note (inj (Proxy :: _ "cookieInfoNotPresent") cookies)
