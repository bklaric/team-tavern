module TeamTavern.Server.Player.ViewSettings.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.ViewSettings.LoadSettings (LoadSettingsResult)
import TeamTavern.Server.Player.ViewSettings.LogError (ViewSettingsError)

type OkContent = LoadSettingsResult

errorResponse :: ViewSettingsError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const unauthorized__
    , notFound: const badRequest__
    , unreadableSettings: const internalServerError__
    , databaseError: const internalServerError__
    }

successResponse :: LoadSettingsResult -> Response
successResponse settings = ok_ $ writeJSON settings

sendResponse
    :: Async ViewSettingsError LoadSettingsResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
