module TeamTavern.Player.View.SendResponse (OkContent, response) where

import Prelude

import Async (Async, alwaysRight)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Perun.Response (Response, internalServerError__, notFound__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Player.View.LoadPlayer (LoadPlayerResult)
import TeamTavern.Player.View.LogError (ViewError)

type OkContent =
    { id :: Int
    , nickname :: String
    , about :: String
    }

errorResponse :: ViewError -> Response
errorResponse = match
    { databaseError: const $ internalServerError__
    , unreadableDto: const $ internalServerError__
    , notFound: const $ notFound__
    }

successResponse :: LoadPlayerResult -> Response
successResponse { id, nickname, about } = ok_ $ writeJSON (
    { id: unwrap id
    , nickname: unwrap nickname
    , about: unwrap about
    } :: OkContent)

response ::
    Async ViewError LoadPlayerResult -> (forall left. Async left Response)
response = alwaysRight errorResponse successResponse
