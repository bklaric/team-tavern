module TeamTavern.Profile.ViewAll.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Profile.ViewAll.LoadProfiles (LoadProfilesResult)
import TeamTavern.Profile.ViewAll.LogError (ViewAllError)

type OkContent = Array
    { handle :: String
    , nickname :: String
    , summary :: String
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { invalidFilters: const badRequest__
    , databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    }

successResponse :: Array LoadProfilesResult -> Response
successResponse profiles = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped profiles \{ nickname, handle, summary } ->
        { handle: unwrap handle
        , nickname: unwrap nickname
        , summary: unwrap summary
        }

sendResponse
    :: Async ViewAllError (Array LoadProfilesResult)
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
