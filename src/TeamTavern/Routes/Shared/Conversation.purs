module TeamTavern.Routes.Shared.Conversation where

import Data.Maybe (Maybe)
import TeamTavern.Routes.Shared.Card (CardRow)

-- | A message as its thread shows it: `content` is its lines, and `mine` whether
-- | the viewer wrote it.
type Message = { mine :: Boolean, content :: Array String, created :: String }

-- | A conversation between a post's owner and one other player, about that
-- | post (brief 10), as either side reads it. `post` is the card with no marks,
-- | whose `own` says whether the viewer is the owner. `otherPost` is, for the
-- | owner, the other player's post in the same game if they have one, player
-- | first. `readTo` is where the viewer had read to before this answer, which
-- | is where the thread's New line goes.
type Conversation =
    { id :: Int
    , game :: { handle :: String, title :: String }
    , post :: CardRow
    , other :: String
    , otherPost :: Maybe CardRow
    , readTo :: Maybe String
    , messages :: Array Message
    }

-- | What a message is sent as, its text as the player typed it.
type MessageContent = { content :: String }
