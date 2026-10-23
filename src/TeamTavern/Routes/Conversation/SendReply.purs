module TeamTavern.Routes.Conversation.SendReply where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NotAuthorized_, NotFound_, OkJson, PostJson_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Conversation (Conversation, MessageContent)
import TeamTavern.Routes.Shared.Wait (Wait)

-- | Writes in a conversation, from either side, and answers with it. Bad
-- | request and not found are as for `SendMessage`. Too many requests is a
-- | message past the viewer's allowance, which `SendMessage` counts too.
type SendReply =
    PostJson_ (Literal "messages" / Capture "id" Int) MessageContent
    ==> OkJson Conversation ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! TooManyRequestsJson Wait ! Internal_
