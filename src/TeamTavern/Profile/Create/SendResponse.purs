module TeamTavern.Profile.Create.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Profile.Create.LogError (CreateError)

type BadRequestContent = Variant
    ( invalidSummary :: {} )

errorResponse :: CreateError -> Response
errorResponse = match
    { cookieInfoNotPresent: const unauthorized__
    , databaseError: const internalServerError__
    , unreadableDto: const badRequest__
    , invalidSummary: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "invalidSummary") {} :: BadRequestContent)
    , notAuthorized: const forbidden__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async CreateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
