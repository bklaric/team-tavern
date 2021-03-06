module TeamTavern.Server.Player.UpdatePlayer.SendResponse (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array (fromFoldable)
import Data.Variant (Variant, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Player.UpdatePlayer.LogError (UpdateDetailsError)

type BadRequestContent = Array (Variant
    ( discordTag :: Array String
    , about :: Array String
    ))

errorResponse :: UpdateDetailsError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    , player: fromFoldable >>> (writeJSON :: BadRequestContent -> String) >>> badRequest_
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse :: Async UpdateDetailsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
