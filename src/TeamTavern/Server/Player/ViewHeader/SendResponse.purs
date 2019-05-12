module TeamTavern.Server.Player.ViewHeader.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.ViewHeader.LoadHeader (LoadHeaderResult)
import TeamTavern.Server.Player.ViewHeader.LogError (ViewHeaderError)

type OkContent = { nickname :: String }

errorResponse :: ViewHeaderError -> Response
errorResponse = match
    { databaseError: const $ internalServerError__
    , unreadableHeader: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: LoadHeaderResult -> Response
successResponse { nickname } = ok_ $ writeJSON (
    { nickname: unwrap nickname } :: OkContent)

sendResponse
    :: Async ViewHeaderError LoadHeaderResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
