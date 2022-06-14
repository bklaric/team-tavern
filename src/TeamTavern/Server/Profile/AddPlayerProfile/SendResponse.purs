module TeamTavern.Server.Profile.AddPlayerProfile.SendResponse (BadContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Array as Array
import Data.Variant (Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, forbidden__, internalServerError__, noContent_, unauthorized__)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsError)
import TeamTavern.Server.Profile.AddPlayerProfile.LogError (CreateError)
import Type (type ($))
import Type.Proxy (Proxy(..))
import Yoga.JSON (writeJSON)

type BadContent = Array $ Variant
    ( profile :: Array $ Variant
        ( about :: Array String
        , ambitions :: Array String
        , url :: { key :: String, message :: Array String }
        )
    , contacts :: Array PlayerContactsError
    )

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
        # (writeJSON :: BadContent -> String)
        # badRequest_
    }

successResponse :: Unit -> Response
successResponse = const noContent_

sendResponse :: Async CreateError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
