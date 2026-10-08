module TeamTavern.Server.Conversation.ViewConversation (viewConversation) where

import Prelude

import Async (Async, note)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Jarilo (InternalRow_, NotAuthorizedRow_, NotFoundRow_, OkRow, notFound__, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Shared.Conversation (Conversation)
import TeamTavern.Server.Conversation.Infrastructure.LoadConversation (loadConversation, markRead)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Row (type (+))

viewConversation :: ∀ left. Pool -> Int -> Cookies
    -> Async left (Variant (OkRow Conversation + NotAuthorizedRow_ + NotFoundRow_ + InternalRow_ + ()))
viewConversation pool id cookies =
    sendResponse "Error viewing conversation" do
    { id: viewer } <- ensureSignedIn pool cookies
    conversation <- loadConversation pool id (unwrap viewer)
        >>= note (Terror notFound__ [ "Can't find conversation " <> show id <> " for player " <> show (unwrap viewer) ])
    markRead pool id (unwrap viewer)
    pure $ ok_ conversation
