module TeamTavern.Server.Conversation.Start where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Conversation.Start.AddMessage (addMessage)
import TeamTavern.Server.Conversation.Start.CreateConversation (createConversation)
import TeamTavern.Server.Conversation.Start.LogError (logError)
import TeamTavern.Server.Conversation.Start.ReadMessage (readMessage)
import TeamTavern.Server.Conversation.Start.SendResponse (sendResponse)
import TeamTavern.Server.Conversation.Start.ValidateMessage (validateMessage)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)

start :: forall left. Pool -> Nickname -> Cookies -> Body -> Async left Response
start pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read message.
    { content } <- readMessage body

    -- Validate message.
    message <- validateMessage content

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Ensure player is signed in.
            cookieInfo <- ensureSignedIn client cookies

            -- Create conversation.
            conversationId <- createConversation client nickname cookieInfo

            -- Add message to conversation.
            addMessage client cookieInfo conversationId message
