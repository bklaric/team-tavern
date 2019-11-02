module TeamTavern.Server.Profile.ViewByGame.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewByGame.LogError (ViewAllError)

type OkContent = Array
    { nickname :: String
    , summary :: Array String
    , fieldValues :: Array
        { fieldKey :: String
        , url :: Maybe String
        , optionKey :: Maybe String
        , optionKeys :: Maybe (Array String)
        }
    , updated :: String
    , updatedSeconds :: Number
   }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    }

successResponse :: Array LoadProfilesResult -> Response
successResponse profiles = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped profiles \{ nickname, summary, fieldValues, updated, updatedSeconds } ->
        { nickname: unwrap nickname
        , summary: unwrap summary <#> unwrap
        , fieldValues
        , updated
        , updatedSeconds
        }

sendResponse
    :: Async ViewAllError (Array LoadProfilesResult)
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
