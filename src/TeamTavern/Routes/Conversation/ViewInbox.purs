module TeamTavern.Routes.Conversation.ViewInbox where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (==>), Get_, Internal_, Literal, NotAuthorized_, OkJson)

-- | The signed-in player's conversations (brief 10, Inbox): those about their
-- | own posts, grouped by post, then those about posts they messaged.
type ViewInbox =
    Get_ (Literal "messages")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

-- | The post a conversation is about, as the inbox names it. `game` is the
-- | game's title.
type InboxPost =
    { id :: Int
    , type :: String
    , name :: Maybe String
    , owner :: String
    , handle :: String
    , game :: String
    , expired :: Boolean
    }

-- | One conversation. `other` is the other side's nickname, and `last` the
-- | latest message with the nickname of whoever wrote it.
type InboxRow =
    { id :: Int
    , post :: InboxPost
    , other :: String
    , last :: { mine :: Boolean, sender :: String, content :: Array String, created :: String }
    , unread :: Boolean
    }

-- | Everything latest first: the rows, and the own posts by their latest row.
type OkContent =
    { own :: Array { post :: InboxPost, conversations :: Array InboxRow }
    , messaged :: Array InboxRow
    }
