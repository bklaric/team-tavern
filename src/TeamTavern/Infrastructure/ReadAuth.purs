module TeamTavern.Infrastructure.ReadAuth where

import Prelude

import Async (Async)
import Async as Async
import Data.Map (Map)
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Infrastructure.Cookie (lookupAuthCookies)
import TeamTavern.Player.Domain.Types (AuthInfo)

type ReadAuthError error = Variant
    ( authNotPresent :: Map String String
    | error )

readAuth :: forall errors.
    Map String String -> Async (ReadAuthError errors) AuthInfo
readAuth cookies = lookupAuthCookies cookies
    # Async.note (inj (SProxy :: SProxy "authNotPresent") cookies)
