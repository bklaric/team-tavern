module TeamTavern.Server.Conversation.ViewAll.SendResponse where

import Prelude

import Async (Async, alwaysRight)
import Data.Variant (match)
import Perun.Response (Response, badRequest__, internalServerError__, ok_, unauthorized__)
import Simple.JSON (writeJSON)
import TeamTavern.Server.Conversation.ViewAll.LoadConversations (LoadConversationsResult)
import TeamTavern.Server.Conversation.ViewAll.LogError (ViewAllError)

type OkContent = LoadConversationsResult

errorResponse :: ViewAllError -> Response
errorResponse = match
    { internal: const internalServerError__
    , client: const badRequest__
    , notAuthenticated: const unauthorized__
    , unreadableResult: const $ internalServerError__
    , databaseError: const $ internalServerError__
    }

successResponse :: LoadConversationsResult -> Response
successResponse conversations = ok_ $ writeJSON conversations

sendResponse
    :: Async ViewAllError LoadConversationsResult
    -> (forall left. Async left Response)
sendResponse = alwaysRight errorResponse successResponse
