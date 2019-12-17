module TeamTavern.Server.Password.Reset.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Password.Reset.LogError (ResetError)

type BadRequestContent = Variant
    ( invalidPassword :: {}
    , invalidNonce :: {}
    )

errorResponse :: ResetError -> Response
errorResponse = match
    { signedIn: const badRequest__
    , unreadableNewPassword: const badRequest__
    , databaseError: const internalServerError__
    , invalidPassword: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "invalidPassword") {}
    , bcryptError: const internalServerError__
    , invalidNonce: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "invalidNonce") {}
    , unreadablePlayer: const internalServerError__
    }

successResponse :: Response
successResponse = noContent_

sendResponse :: Async ResetError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse (const successResponse)
