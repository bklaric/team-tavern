module TeamTavern.Game.View.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Game.View.LoadGame (LoadGameResult)
import TeamTavern.Game.View.LogError (ViewError)

type OkContent =
    { administratorId :: Int
    , title :: String
    , handle :: String
    , description :: String
    , hasProfile :: Boolean
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { databaseError: const $ internalServerError__
    , unreadableDto: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: LoadGameResult -> Response
successResponse { administratorId, title, handle, description, hasProfile } =
    ok_ $ writeJSON (
    { administratorId: unwrap administratorId
    , title: unwrap title
    , handle: unwrap handle
    , description: unwrap description
    , hasProfile
    } :: OkContent)

sendResponse ::
    Async ViewError LoadGameResult -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
