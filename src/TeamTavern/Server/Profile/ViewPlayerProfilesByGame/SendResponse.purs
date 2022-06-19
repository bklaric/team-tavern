module TeamTavern.Server.Profile.ViewPlayerProfilesByGame.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import TeamTavern.Routes.Profile.ViewPlayerProfilesByGame as ViewPlayerProfilesByGame
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame.LogError (ViewAllError)
import Yoga.JSON (writeJSON)

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    , unreadableCount: const internalServerError__
    , noRowsSomehow: const internalServerError__
    }

successResponse :: ViewPlayerProfilesByGame.OkContent -> Response
successResponse result = ok_ $ writeJSON result

sendResponse ::
    Async ViewAllError ViewPlayerProfilesByGame.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
