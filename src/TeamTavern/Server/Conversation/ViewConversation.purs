module TeamTavern.Server.Conversation.ViewConversation (viewConversation) where

import Prelude

import Async (Async, note)
import Data.Newtype (unwrap)
import Jarilo (notFound__, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Server.Conversation.Infrastructure.LoadConversation (loadConversation, markRead)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

viewConversation :: ∀ left. Pool -> Int -> Cookies -> Async left _
viewConversation pool id cookies =
    sendResponse "Error viewing conversation" do
    { id: viewer } <- ensureSignedIn pool cookies
    conversation <- loadConversation pool id (unwrap viewer)
        >>= note (Terror notFound__ [ "Can't find conversation " <> show id <> " for player " <> show (unwrap viewer) ])
    markRead pool id (unwrap viewer)
    pure $ ok_ conversation
