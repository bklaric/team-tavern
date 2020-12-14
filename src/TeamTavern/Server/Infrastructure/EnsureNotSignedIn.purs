module TeamTavern.Server.Infrastructure.EnsureNotSignedIn where

import Prelude

import Async (Async)
import Async as Async
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, Cookies, lookupCookieInfo)

type EnsureNotSignedInError errors = Variant
    ( signedIn ::
        { cookieInfo :: CookieInfo
        , cookies :: Map String String
        }
    | errors )

ensureNotSignedIn :: forall errors. Cookies -> Async (EnsureNotSignedInError errors) Unit
ensureNotSignedIn cookies =
    case lookupCookieInfo cookies of
    Nothing -> Async.right unit
    Just cookieInfo -> Async.left $ inj (SProxy :: SProxy "signedIn") { cookieInfo, cookies }

ensureNotSignedIn' :: forall errors.
    Cookies -> Async (Variant (signedIn :: Array String | errors)) Unit
ensureNotSignedIn' cookies =
    case lookupCookieInfo cookies of
    Nothing -> Async.right unit
    Just cookieInfo -> Async.left $ inj (SProxy :: SProxy "signedIn")
        [ "Expected user to be signed out, but he's signed in with credentials: "
        <> show cookieInfo
        ]
