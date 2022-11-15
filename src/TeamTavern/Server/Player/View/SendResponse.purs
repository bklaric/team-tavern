module TeamTavern.Server.Player.View.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Yoga.JSON (writeJSON)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Server.Infrastructure.Error (LoadSingleError)

errorResponse :: LoadSingleError () -> Response
errorResponse = match
    { internal: const internalServerError__
    , notFound: const notFound__
    }

successResponse :: ViewPlayer.OkContent -> Response
successResponse account = ok_ $ writeJSON account

sendResponse ::
    Async (LoadSingleError ()) ViewPlayer.OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
