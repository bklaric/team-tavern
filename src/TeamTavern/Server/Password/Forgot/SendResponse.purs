module TeamTavern.Server.Password.Forgot.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Password.Forgot.LogError (ForgotError)

type BadRequestContent = Variant (notFound :: {})

errorResponse :: ForgotError -> Response
errorResponse = match
    { signedIn: const badRequest__
    , unreadableEmailAddress: const badRequest__
    , randomError: const internalServerError__
    , databaseError: const internalServerError__
    , notFound: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "notFound") {}
    , unreadablePlayer: const internalServerError__
    , sendEmailError: const internalServerError__
    }

successResponse :: Response
successResponse = noContent_

sendResponse :: Async ForgotError Unit -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse (const successResponse)
