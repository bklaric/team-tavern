module TeamTavern.Session.Start.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.MultiMap (singleton)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest, badRequest__, forbidden__, internalServerError__, noContent)
import Simple.JSON (writeJSON)
import TeamTavern.Infrastructure.Cookie (setCookieHeader)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Session.Domain.Token (Token)
import TeamTavern.Session.Start.LogError (StartError)

type SendResponseModel =
    { id :: Id
    , token :: Token
    }

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
        (singleton "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "noSessionStarted") {} :: BadRequestContent)
    , passwordDoesntMatch: const $ badRequest
        (singleton "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "noSessionStarted") {} :: BadRequestContent)
    , unconfirmedEmail: const $ badRequest
        (singleton "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "unconfirmedEmail") {} :: BadRequestContent)
    , nothingConfirmed: const $ badRequest
        (singleton "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "nothingConfirmed") {} :: BadRequestContent)
    , noSessionStarted: const $ badRequest
        (singleton "Content-Type" "application/json") $ writeJSON
        (inj (SProxy :: SProxy "noSessionStarted") {} :: BadRequestContent)
    }

successResponse :: SendResponseModel -> Response
successResponse { id, token } = noContent $ setCookieHeader id token

sendResponse ::
    Async StartError SendResponseModel -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
