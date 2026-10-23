module TeamTavern.Routes.Conversation.SendMessage where

import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequest_, Capture, Internal_, Literal, NotAuthorized_, NotFound_, OkJson, PostJson_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Conversation (Conversation, MessageContent)
import TeamTavern.Routes.Shared.Wait (Wait)

-- | Writes to a post's owner, starting the conversation about the post with the
-- | first message (brief 10), and answers with the conversation. Bad request is
-- | a message with nothing in it or too long. Not found is a post that isn't the
-- | game's, the viewer's own, or one either side has blocked the other from.
-- | Too many requests is a message past the viewer's allowance, or a
-- | conversation past it.
type SendMessage =
    PostJson_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "messages") MessageContent
    ==> OkJson Conversation ! BadRequest_ ! NotAuthorized_ ! NotFound_ ! TooManyRequestsJson TooManyContent ! Internal_

-- | The seconds until the viewer can send another message, or until they can
-- | start another conversation, along with whether their email is confirmed,
-- | which would let them start more.
type TooManyContent = Variant
    ( messages :: Wait
    , conversations :: { seconds :: Int, confirmed :: Boolean }
    )
