module TeamTavern.Server.Conversation.SendReply (sendReply) where

import Prelude

import Async (Async, foreach)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Jarilo (BadRequestRow_, InternalRow_, NotAuthorizedRow_, NotFoundRow_, OkRow, TooManyRequestsRow, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Shared.Conversation (Conversation, MessageContent)
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Conversation.Infrastructure.PostMessage (postMessage, validateMessage)
import TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (sendMessageEmail)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter, ensureAdmitted)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Row (type (+))

sendReply :: ∀ left. Limiter -> Mailer -> Pool -> Int -> Cookies -> MessageContent
    -> Async left (Variant (OkRow Conversation + BadRequestRow_ + NotAuthorizedRow_ + NotFoundRow_ + TooManyRequestsRow Wait + InternalRow_ + ()))
sendReply messageLimit mailer pool id cookies { content } =
    sendResponse "Error sending reply" do
    { id: viewer } <- ensureSignedIn pool cookies
    lines <- validateMessage content
    ensureAdmitted messageLimit (show $ unwrap viewer) "Too many messages sent by one account."
    { conversation, email } <- pool # transaction \client -> postMessage client id (unwrap viewer) lines
    foreach email $ sendMessageEmail mailer
    pure $ ok_ conversation
