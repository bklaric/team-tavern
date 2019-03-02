module TeamTavern.Player.ViewHeader.SendResponse (OkContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.ViewHeader.LoadHeader (LoadPlayerHeaderResult)
import TeamTavern.Player.ViewHeader.LogError (ViewHeaderError)

type OkContent = { nickname :: String }

errorResponse :: ViewHeaderError -> Response
errorResponse = match
    { databaseError: const $ internalServerError__
    , unreadableHeader: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: LoadPlayerHeaderResult -> Response
successResponse { nickname } = ok_ $ writeJSON (
    { nickname: unwrap nickname } :: OkContent)

sendResponse
    :: Async ViewHeaderError LoadPlayerHeaderResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
