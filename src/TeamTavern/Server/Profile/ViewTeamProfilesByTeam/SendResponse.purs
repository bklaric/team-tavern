module TeamTavern.Server.Profile.ViewTeamProfilesByTeam.SendResponse
    (OkContent, OkContent', sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewTeamProfilesByTeam.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewTeamProfilesByTeam.LogError (ViewAllError)

type OkContent = Array LoadProfilesResult

type OkContent' = LoadProfilesResult

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
