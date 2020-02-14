module TeamTavern.Server.Profile.ViewByGame.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Maybe (Maybe)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewByGame.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewByGame.LogError (ViewAllError)
import Unsafe.Coerce (unsafeCoerce)

type OkContent =
    { profiles :: Array
        { nickname :: String
        , age :: Maybe Int
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
    , count :: Int
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    , unreadableCount: const internalServerError__
    , noRowsSomehow: const internalServerError__
    }

successResponse :: { profiles :: Array LoadProfilesResult, count :: Int } -> Response
successResponse result = ok_ $ (writeJSON :: OkContent -> String) $ unsafeCoerce result

sendResponse
    :: Async ViewAllError { profiles :: Array LoadProfilesResult, count :: Int }
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
