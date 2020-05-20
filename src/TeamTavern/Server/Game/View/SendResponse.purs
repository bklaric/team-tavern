module TeamTavern.Server.Game.View.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Game.View.LoadGame (LoadGameResult)
import TeamTavern.Server.Game.View.LogError (ViewError)

type OkContent =
    { administratorId :: Int
    , title :: String
    , handle :: String
    , description :: Array String
    , hasPlayerProfile :: Boolean
    , hasTeamProfile :: Boolean
    , fields :: Array
        { ilk :: Int
        , label :: String
        , key :: String
        , icon :: String
        , required :: Boolean
        , domain :: Maybe String
        , options :: Maybe (Array
            { key :: String
            , label :: String
            })
        }
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { databaseError: const $ internalServerError__
    , unreadableDto: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: LoadGameResult -> Response
successResponse
    { administratorId, title, handle, description, hasPlayerProfile, hasTeamProfile, fields } =
    ok_ $ writeJSON (
    { administratorId: unwrap administratorId
    , title: unwrap title
    , handle: unwrap handle
    , description: unwrap description <#> unwrap
    , hasPlayerProfile
    , hasTeamProfile
    , fields
    } :: OkContent)

sendResponse ::
    Async ViewError LoadGameResult -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
