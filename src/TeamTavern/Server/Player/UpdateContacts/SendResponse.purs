module TeamTavern.Server.Player.UpdateContacts.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Yoga.JSON (writeJSON)
import TeamTavern.Server.Player.UpdateContacts.LogError (UpdateContactsError)

errorResponse :: UpdateContactsError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    , playerContacts: Array.fromFoldable >>> writeJSON >>> badRequest_
    }

successResponse :: Unit -> Response
successResponse _ = noContent_

sendResponse :: Async UpdateContactsError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
