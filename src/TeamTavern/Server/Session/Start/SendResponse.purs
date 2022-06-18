module TeamTavern.Server.Session.Start.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.MultiMap (singleton')
import Data.Variant (inj, match)
import Perun.Response (Response, badRequest, badRequest__, forbidden__, internalServerError__, noContent)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Architecture.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, setCookieHeaderFull)
import TeamTavern.Server.Session.Start.LogError (StartError)
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeJSON)

errorResponse :: StartError -> Response
errorResponse = match
    { internal: const internalServerError__
    , signedIn: const forbidden__
    , unreadableDto: const badRequest__
    , bcrypt: const internalServerError__
    , randomError: const internalServerError__
    , databaseError: const internalServerError__
    , unreadableHash: const internalServerError__
    , noMatchingPlayer: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)
    , passwordDoesntMatch: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)
    , noSessionStarted: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (Proxy :: _ "noSessionStarted") {} :: StartSession.BadContent)
    }

successResponse :: Deployment -> CookieInfo -> Response
successResponse deployment cookieInfo = noContent $ setCookieHeaderFull deployment cookieInfo

sendResponse :: Deployment -> Async StartError CookieInfo -> (forall left. Async left Response)
sendResponse deployment = alwaysRight errorResponse $ successResponse deployment
