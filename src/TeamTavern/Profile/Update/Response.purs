module TeamTavern.Profile.Update.Response where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, notFound__, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Profile.Update.Types (UpdateError)

type BadRequestContent = Variant
    ( invalidSummary :: {} )

errorResponse :: UpdateError -> Response
errorResponse = match
    { invalidIdentifiers: const notFound__
    , authNotPresent: const unauthorized__
    , databaseError: const internalServerError__
    , unreadableSummary: const badRequest__
    , invalidSummary: const $ badRequest_ $ writeJSON $
        (inj (SProxy :: SProxy "invalidSummary") {} :: BadRequestContent)
    , notAuthorized: const forbidden__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

response :: Async UpdateError Unit -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
