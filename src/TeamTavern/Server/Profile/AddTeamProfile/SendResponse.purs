module TeamTavern.Server.Profile.AddTeamProfile.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.AddTeamProfile.LogError (AddProfileError)

errorResponse :: AddProfileError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthorized: const forbidden__
    , profile: badRequest_ <<< writeJSON <<< Array.fromFoldable
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async AddProfileError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
