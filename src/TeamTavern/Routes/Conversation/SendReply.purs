module TeamTavern.Routes.Conversation.SendReply where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NotAuthorized_, NotFound_, OkJson, PostJson_)
import TeamTavern.Routes.Shared.Conversation (Conversation, MessageContent)

-- | Writes in a conversation, from either side, and answers with it. Bad
-- | request and not found are as for `SendMessage`.
type SendReply =
    PostJson_ (Literal "messages" / Capture "id" Int) MessageContent
    ==> OkJson Conversation ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! Internal_
