module TeamTavern.Server.Conversation.View.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Conversation.View.LoadConversation (LoadConversationResult)
import TeamTavern.Server.Conversation.View.LogError (ViewError)

type OkContent = LoadConversationResult

errorResponse :: ViewError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , unreadableResult: const $ internalServerError__
    , databaseError: const $ internalServerError__
    }

successResponse :: LoadConversationResult -> Response
successResponse conversations = ok_ $ writeJSON conversations

sendResponse
    :: Async ViewError LoadConversationResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
