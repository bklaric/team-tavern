module TeamTavern.Server.Infrastructure.EnsureNotSignedIn where

import Prelude

import Async (Async, left, right)
import Data.Maybe (Maybe(..))
import Jarilo (forbidden__)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, lookupCookieInfo)
import TeamTavern.Server.Infrastructure.Error (ForbiddenError_, TavernError(..))

ensureNotSignedIn :: forall errors. Cookies -> Async (ForbiddenError_ errors) Unit
ensureNotSignedIn cookies =
    case lookupCookieInfo cookies of
    Nothing -> right unit
    Just cookieInfo -> left $ TavernError forbidden__
        [ "Expected user to be signed out, but he's signed in with credentials: " <> show cookieInfo
        , "Got these credentials from cookies: " <> show cookies
        ]
