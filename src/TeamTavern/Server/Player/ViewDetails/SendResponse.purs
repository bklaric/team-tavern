module TeamTavern.Server.Player.ViewDetails.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, forbidden__, internalServerError__, ok_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.ViewDetails.LoadDetails (LoadDetailsResult)
import TeamTavern.Server.Player.ViewDetails.LogError (ViewDetailsError)

type OkContent = LoadDetailsResult

errorResponse :: ViewDetailsError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const forbidden__
    , notFound: const badRequest__
    , unreadableAccount: const internalServerError__
    , databaseError: const internalServerError__
    }

successResponse :: LoadDetailsResult -> Response
successResponse account = ok_ $ writeJSON account

sendResponse
    :: Async ViewDetailsError LoadDetailsResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
