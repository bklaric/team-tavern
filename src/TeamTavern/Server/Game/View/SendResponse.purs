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
    { title :: String
    , handle :: String
    , fields :: Array
        { type :: Int
        , label :: String
        , key :: String
        , options :: Maybe (Array
            { key :: String
            , option :: String
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
successResponse { title, handle, fields } =
    ok_ $ (writeJSON :: OkContent -> String)
        { title: unwrap title
        , handle: unwrap handle
        , fields
        }

sendResponse ::
    Async ViewError LoadGameResult -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
