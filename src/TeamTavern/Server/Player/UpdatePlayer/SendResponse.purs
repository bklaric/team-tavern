module TeamTavern.Server.Player.UpdatePlayer.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import TeamTavern.Server.Player.UpdatePlayer.LogError (UpdateDetailsError)

errorResponse :: UpdateDetailsError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse :: Async UpdateDetailsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
