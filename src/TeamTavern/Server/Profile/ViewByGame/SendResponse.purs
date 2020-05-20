module TeamTavern.Server.Profile.ViewByGame.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewByGame.LogError (ViewAllError)

type OkContent =
    { profiles :: Array LoadProfilesResult
    , count :: Int
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    , unreadableCount: const internalServerError__
    , noRowsSomehow: const internalServerError__
    }

successResponse :: OkContent -> Response
successResponse result = ok_ $ (writeJSON :: OkContent -> String) result

sendResponse ::
    Async ViewAllError OkContent -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
