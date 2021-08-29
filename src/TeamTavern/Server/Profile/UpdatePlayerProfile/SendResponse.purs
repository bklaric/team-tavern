module TeamTavern.Server.Profile.UpdatePlayerProfile.SendResponse (BadContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Routes.Shared.Player (ContactsError)
import TeamTavern.Server.Profile.UpdatePlayerProfile.LogError (UpdateError)
import Type (type ($))

type BadContent = Array $ Variant
    ( profile :: Array (Variant
                ( about :: Array String
                , url :: { key :: String, message :: Array String }
                ))
    , contacts :: Array ContactsError
    )

errorResponse :: UpdateError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , profile: badRequest_ <<< writeJSON <<< Array.fromFoldable
    , cookieInfoNotPresent: const unauthorized__
    , databaseError: const internalServerError__
    , unreadableFields: const internalServerError__
    , unreadableProfile: const badRequest__
    , invalidBody: \errors ->
        errors
        # Array.fromFoldable
        <#> (match
            { playerProfile: Array.fromFoldable >>> inj (SProxy :: _ "profile")
            , playerContacts: Array.fromFoldable >>> inj (SProxy :: _ "contacts")
            })
        # (writeJSON :: BadContent -> String)
        # badRequest_
    , notAuthorized: const forbidden__
    , unreadableProfileId: const internalServerError__
    , emptyResult: const internalServerError__
    , unreadableFieldValueId: const internalServerError__
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async UpdateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
