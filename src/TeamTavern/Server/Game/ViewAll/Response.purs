module TeamTavern.Server.Game.ViewAll.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Yoga.JSON (writeJSON)
import TeamTavern.Routes.Game.ViewAllGames as ViewAllGame
import TeamTavern.Server.Game.ViewAll.LogError (ViewAllError)

errorResponse :: ViewAllError -> Response
errorResponse = match { internal: const $ internalServerError__ }

successResponse :: ViewAllGame.OkContent -> Response
successResponse games  = ok_ $ writeJSON games

sendResponse :: Async ViewAllError ViewAllGame.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
