module TeamTavern.Server.Player.EditSettings.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, noContent_, unauthorized__)
import TeamTavern.Server.Player.EditSettings.LogError (EditSettingsError)

errorResponse :: EditSettingsError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , databaseError: const $ internalServerError__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const unauthorized__
    , unreadableModel: const $ badRequest__
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse ::
    Async EditSettingsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
