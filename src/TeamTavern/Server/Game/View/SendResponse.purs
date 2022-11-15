module TeamTavern.Server.Game.View.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Yoga.JSON (writeJSON)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Server.Game.View.LogError (ViewError)

errorResponse :: ViewError -> Response
errorResponse = match
    { internal: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: ViewGame.OkContent -> Response
successResponse result = ok_ $ writeJSON result

sendResponse :: Async ViewError ViewGame.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
