module TeamTavern.Server.Game.ViewAll.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Functor (mapFlipped)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Game.ViewAll.LoadGames (LoadGamesResult)
import TeamTavern.Server.Game.ViewAll.LogError (ViewAllError)

type OkContent = Array
    { administratorId :: Int
    , title :: String
    , handle :: String
    , description :: Array String
    }

errorResponse :: ViewAllError -> Response
errorResponse = match
    { unreadableDtos: const $ internalServerError__
    , databaseError: const $ internalServerError__
    }

successResponse :: Array LoadGamesResult -> Response
successResponse views  = ok_ $ (writeJSON :: OkContent -> String) $
    mapFlipped views \{ administratorId, title, handle, description } ->
        { administratorId: unwrap administratorId
        , title: unwrap title
        , handle: unwrap handle
        , description: unwrap description <#> unwrap
        }

sendResponse
    :: Async ViewAllError (Array LoadGamesResult)
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
