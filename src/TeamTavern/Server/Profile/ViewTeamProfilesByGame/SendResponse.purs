module TeamTavern.Server.Profile.ViewTeamProfilesByGame.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame as ViewTeamProfilesByGame
import TeamTavern.Server.Profile.ViewTeamProfilesByGame.LogError (ViewAllError)
import Yoga.JSON (writeJSON)

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    , unreadableCount: const internalServerError__
    , noRowsSomehow: const internalServerError__
    }

successResponse :: ViewTeamProfilesByGame.OkContent -> Response
successResponse result = ok_ $ writeJSON result

sendResponse ::
    Async ViewAllError ViewTeamProfilesByGame.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
