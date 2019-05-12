module TeamTavern.Server.Infrastructure.EnsureNotSignedIn where

import Prelude

import Async (Async)
import Async as Async
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, lookupCookieInfo)

type EnsureNotSignedInError errors = Variant
    ( signedIn ::
        { cookieInfo :: CookieInfo
        , cookies :: Map String String
        }
    | errors )

ensureNotSignedIn :: forall errors.
    Map String String -> Async (EnsureNotSignedInError errors) Unit
ensureNotSignedIn cookies =
    lookupCookieInfo cookies
    # case _ of
        Nothing -> Async.right unit
        Just cookieInfo -> Async.left
            $ inj (SProxy :: SProxy "signedIn") { cookieInfo, cookies }
