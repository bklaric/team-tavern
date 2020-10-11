module TeamTavern.Server.Player.EditSettings.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, forbidden__, internalServerError__, noContent_)
import TeamTavern.Server.Player.EditSettings.LogError (EditSettingsError)

errorResponse :: EditSettingsError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , databaseError: const $ internalServerError__
    , nicknameDoesntMatch: const forbidden__
    , unreadableModel: const $ badRequest__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse ::
    Async EditSettingsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
