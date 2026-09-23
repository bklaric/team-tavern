module TeamTavern.Server.Conversation.SendReply (sendReply) where

import Prelude

import Async (Async, foreach)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Shared.Conversation (MessageContent)
import TeamTavern.Server.Conversation.Infrastructure.PostMessage (postMessage, validateMessage)
import TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (sendMessageEmail)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

sendReply :: ∀ left. Mailer -> Pool -> Int -> Cookies -> MessageContent -> Async left _
sendReply mailer pool id cookies { content } =
    sendResponse "Error sending reply" do
    { id: viewer } <- ensureSignedIn pool cookies
    lines <- validateMessage content
    { conversation, email } <- pool # transaction \client -> postMessage client id (unwrap viewer) lines
    foreach email $ sendMessageEmail mailer
    pure $ ok_ conversation
