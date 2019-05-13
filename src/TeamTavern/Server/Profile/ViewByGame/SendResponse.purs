module TeamTavern.Server.Profile.ViewByGame.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewByGame.LogError (ViewAllError)

type OkContent = Array
    { nickname :: String
    , summary :: Array String
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    }

successResponse :: Array LoadProfilesResult -> Response
successResponse profiles = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped profiles \{ nickname, summary } ->
        { nickname: unwrap nickname
        , summary: unwrap summary <#> unwrap
        }

sendResponse
    :: Async ViewAllError (Array LoadProfilesResult)
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse