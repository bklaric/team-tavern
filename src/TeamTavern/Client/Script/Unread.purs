module TeamTavern.Client.Script.Unread (announceUnread, onUnread) where

import Prelude

import Effect (Effect)

-- | Tells the header that what the player has unread may have changed, as a
-- | page that reads a conversation or sends a message knows before the
-- | header's own asking on each visit does.
foreign import announceUnread :: Effect Unit

-- | Calls back each time a page announces it, until the effect returned stops it.
foreign import onUnread :: Effect Unit -> Effect (Effect Unit)
