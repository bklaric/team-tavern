module TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.SendResponse
    (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.LogError (ViewAllError)

type OkContent = Array LoadProfilesResult

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    }

successResponse :: Array LoadProfilesResult -> Response
successResponse profiles = ok_ $ writeJSON profiles

sendResponse
    :: Async ViewAllError (Array LoadProfilesResult)
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
