module TeamTavern.Routes.Conversation.ViewPostConversation where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, NotAuthorized_, OkJson)
import TeamTavern.Routes.Shared.Conversation (Conversation)

-- | The viewer's conversation about a post, for its contact panel, which
-- | reading marks read. Nothing where the viewer hasn't written about it.
type ViewPostConversation =
    Get_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "messages")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

type OkContent = { conversation :: Maybe Conversation }
