module TeamTavern.Routes.Conversation.ViewConversation where

import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, NotAuthorized_, NotFound_, OkJson)
import TeamTavern.Routes.Shared.Conversation (Conversation)

-- | A conversation in the inbox, which reading marks read for the viewer. Not
-- | found is one the viewer isn't a side of.
type ViewConversation =
    Get_ (Literal "messages" / Capture "id" Int)
    ==> OkJson Conversation ! NotAuthorized_ ! NotFound_ ! Internal_
