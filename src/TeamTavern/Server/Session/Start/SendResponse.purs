module TeamTavern.Server.Session.Start.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.MultiMap (singleton')
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest, badRequest__, forbidden__, internalServerError__, noContent)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo, setCookieHeader)
import TeamTavern.Server.Session.Start.LogError (StartError)

type BadRequestContent = Variant
    ( unconfirmedEmail :: {}
    , nothingConfirmed :: {}
    , noSessionStarted :: {}
    )

errorResponse :: StartError -> Response
errorResponse = match
    { signedIn: const forbidden__
    , unreadableDto: const badRequest__
    , bcryptError: const internalServerError__
    , randomError: const internalServerError__
    , databaseError: const internalServerError__
    , unreadableHash: const internalServerError__
    , noMatchingPlayer: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "noSessionStarted") {} :: BadRequestContent)
    , passwordDoesntMatch: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "noSessionStarted") {} :: BadRequestContent)
    , unconfirmedEmail: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "unconfirmedEmail") {} :: BadRequestContent)
    , nothingConfirmed: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "nothingConfirmed") {} :: BadRequestContent)
    , noSessionStarted: const $ badRequest
        (singleton' "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "noSessionStarted") {} :: BadRequestContent)
    }

successResponse :: CookieInfo -> Response
successResponse cookieInfo = noContent $ setCookieHeader cookieInfo

sendResponse ::
    Async StartError CookieInfo -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
