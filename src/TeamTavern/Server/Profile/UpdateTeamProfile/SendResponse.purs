module TeamTavern.Server.Profile.UpdateTeamProfile.SendResponse (BadContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsError)
import TeamTavern.Server.Profile.UpdateTeamProfile.LogError (UpdateProfileError)
import Type (type ($))
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeJSON)

type BadContent = Array $ Variant
    ( profile :: Array $ Variant
        ( platforms :: Array String
        , about :: Array String
        , ambitions :: Array String
        )
    , contacts :: Array TeamContactsError
    )

errorResponse :: UpdateProfileError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    , invalidBody: \errors ->
        errors
        # Array.fromFoldable
        <#> (match
            { teamProfile: Array.fromFoldable >>> inj (Proxy :: _ "profile")
            , teamContacts: Array.fromFoldable >>> inj (Proxy :: _ "contacts")
            })
        # (writeJSON :: BadContent -> String)
        # badRequest_
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async UpdateProfileError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
