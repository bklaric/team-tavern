module TeamTavern.Server.Profile.AddPlayerProfile.SendResponse (sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Server.Profile.AddPlayerProfile.LogError (CreateError)
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeJSON)

errorResponse :: CreateError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , notAuthorized: const forbidden__
    , invalidBody: \errors ->
        errors
        # Array.fromFoldable
        <#> (match
            { playerProfile: Array.fromFoldable >>> inj (Proxy :: _ "profile")
            , playerContacts: Array.fromFoldable >>> inj (Proxy :: _ "contacts")
            })
        # (writeJSON :: AddPlayerProfile.BadContent -> String)
        # badRequest_
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async CreateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
