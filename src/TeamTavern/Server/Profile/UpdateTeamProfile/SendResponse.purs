module TeamTavern.Server.Profile.UpdateTeamProfile.SendResponse
    (ProfileErrorContent, BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (Variant, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.UpdateTeamProfile.LogError (AddGameTeamError)

type ProfileErrorContent = Variant
    ( invalidSummary :: {} )

type BadRequestContent = Variant (invalidProfile :: Array ProfileErrorContent)

errorResponse :: AddGameTeamError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , profile: badRequest_ <<< writeJSON <<< Array.fromFoldable
    , databaseError: const $ internalServerError__
    , nicknameDoesntMatch: const forbidden__
    , unreadableFields: const internalServerError__
    , unreadableProfile: const badRequest__
    , nothingInserted: const internalServerError__
    , unreadableProfileId: const internalServerError__
    , emptyResult: const internalServerError__
    , unreadableFieldValueId: const internalServerError__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async AddGameTeamError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
