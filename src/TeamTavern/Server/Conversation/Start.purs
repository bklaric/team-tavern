module TeamTavern.Server.Conversation.Start where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import Postmark.Client (Client)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Conversation.Start.AddMessage (addMessage)
import TeamTavern.Server.Conversation.Start.CreateConversation (createConversation)
import TeamTavern.Server.Conversation.Start.LogError (logError)
import TeamTavern.Server.Conversation.Start.NotifyReceiver (notifyReceiver)
import TeamTavern.Server.Conversation.Start.ReadMessage (readMessage)
import TeamTavern.Server.Conversation.Start.SendResponse (sendResponse)
import TeamTavern.Server.Conversation.Start.ValidateMessage (validateMessage)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Player.Infrastructure.LoadPlayerInfo (loadPlayerInfo)

start :: forall left.
    Pool -> Maybe Client -> Nickname -> Cookies -> Body -> Async left Response
start pool client nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read message.
    { content } <- readMessage body

    -- Validate message.
    message <- validateMessage content

    senderInfo <-
        pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client' -> do
            -- Ensure player is signed in.
            cookieInfo <- ensureSignedIn client' cookies

            -- Create conversation.
            conversationId <- createConversation client' nickname cookieInfo

            -- Add message to conversation.
            addMessage client' cookieInfo conversationId message

            pure cookieInfo

    -- Load receiver info.
    receiverInfo <- loadPlayerInfo pool nickname

    -- Notify receiver about the message.
    if receiverInfo.notify
    then
        notifyReceiver client
            { sender: unwrap senderInfo.nickname
            , receiver: receiverInfo.nickname
            , receiverEmail: receiverInfo.email
            }
    else
        pure unit
