module TeamTavern.Routes.Conversation.SendMessage where

import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NotAuthorized_, NotFound_, OkJson, PostJson_)
import TeamTavern.Routes.Shared.Conversation (Conversation, MessageContent)

-- | Writes to a post's owner, starting the conversation about the post with the
-- | first message (brief 10), and answers with the conversation. Bad request is
-- | a message with nothing in it or too long. Not found is a post that isn't the
-- | game's, the viewer's own, or one either side has blocked the other from.
type SendMessage =
    PostJson_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "messages") MessageContent
    ==> OkJson Conversation ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! Internal_
