module TeamTavern.Server.Conversation.Start.SendResponse
    (BadRequestContent, sendResponse) where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (SProxy(..), Variant, inj, match)
import Perun.Response (Response, badRequest_, badRequest__, internalServerError__, noContent_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Conversation.Start.LogError (StartError)

type BadRequestContent = Variant (invalidMessage :: {})

errorResponse :: StartError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , databaseError: const internalServerError__
    , unreadableConversationId: const badRequest__
    , nothingInsertedSomehow: const internalServerError__
    , unreadableMessage: const badRequest__
    , invalidMessage: const $ badRequest_
        $ (writeJSON :: BadRequestContent -> String)
        $ inj (SProxy :: SProxy "invalidMessage") {}
    , unreadableResult: const noContent_
    , notFound: const noContent_
    , sendEmailError: const noContent_
    }

successResponse :: Response
successResponse = noContent_

sendResponse
    :: Async StartError Unit
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse (const successResponse)
