module TeamTavern.Server.Profile.AddTeamProfile.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Profile.AddTeamProfile.LogError (AddGameTeamError)

type ProfileErrorContent = Variant
    ( invalidSummary :: {} )

type BadRequestContent = Variant (invalidProfile :: Array ProfileErrorContent)

errorResponse :: AddGameTeamError -> Response
errorResponse = match
    { noCookieInfo: const unauthorized__
    , databaseError: const $ internalServerError__
    , invalidSession: const unauthorized__
    , nicknameDoesntMatch: const forbidden__
    , unreadableFields: const internalServerError__
    , unreadableProfile: const badRequest__
    , invalidProfile: \{ errors } ->
        errors
        # Array.fromFoldable
        <#> match
            { summary: const $ Array.singleton
                $ inj (SProxy :: SProxy "invalidSummary") {}
            }
        # join
        # inj (SProxy :: SProxy "invalidProfile")
        # (writeJSON :: BadRequestContent -> String)
        # badRequest_
    , nothingInserted: const internalServerError__
    , unreadableProfileId: const internalServerError__
    , emptyResult: const internalServerError__
    , unreadableFieldValueId: const internalServerError__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async AddGameTeamError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
