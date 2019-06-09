module TeamTavern.Server.Profile.ViewByPlayer.SendResponse
    (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.ViewByPlayer.LoadProfiles (LoadProfilesResult)
import TeamTavern.Server.Profile.ViewByPlayer.LogError (ViewAllError)

type OkContent = Array
    { handle :: String
    , title :: String
    , summary :: Array String
    , fieldValues :: Array
        { id :: Int
        , fieldId :: Int
        , url :: Maybe String
        , optionId :: Maybe Int
        , optionIds :: Maybe (Array Int)
        }
    , fields :: Array
        { id :: Int
        , type :: Int
        , label :: String
        , options :: Maybe (Array
            { id :: Int
            , option :: String
            })
        }
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { databaseError: const internalServerError__
    , unreadableDtos: const internalServerError__
    }

successResponse :: Array LoadProfilesResult -> Response
successResponse profiles = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped profiles \{ title, handle, summary, fieldValues, fields } ->
        { handle: unwrap handle
        , title: unwrap title
        , summary: unwrap summary <#> unwrap
        , fieldValues
        , fields
        }

sendResponse
    :: Async ViewAllError (Array LoadProfilesResult)
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
