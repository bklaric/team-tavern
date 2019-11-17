module TeamTavern.Server.Conversation.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Conversation.View.LoadConversation (loadConversation)
import TeamTavern.Server.Conversation.View.LogError (logError)
import TeamTavern.Server.Conversation.View.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)

view :: forall left. Pool -> Nickname -> Cookies -> Async left Response
view pool nickname cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure player is signed in.
    cookieInfo <- ensureSignedIn pool cookies

    -- Load specified conversations.
    loadConversation pool nickname cookieInfo
