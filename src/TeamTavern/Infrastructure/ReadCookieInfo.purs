module TeamTavern.Infrastructure.ReadCookieInfo where

import Prelude

import Async (Async)
import Async as Async
import Data.Map (Map)
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Infrastructure.Cookie (CookieInfo, lookupCookieInfo)

type ReadCookieInfoError error = Variant
    ( cookieInfoNotPresent :: Map String String
    | error )

readCookieInfo :: forall errors.
    Map String String -> Async (ReadCookieInfoError errors) CookieInfo
readCookieInfo cookies = lookupCookieInfo cookies
    # Async.note (inj (SProxy :: SProxy "cookieInfoNotPresent") cookies)
