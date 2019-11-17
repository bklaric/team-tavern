module TeamTavern.Server.Conversation.ViewAll where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Conversation.ViewAll.LoadConversations (loadConversations)
import TeamTavern.Server.Conversation.ViewAll.LogError (logError)
import TeamTavern.Server.Conversation.ViewAll.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)

viewAll :: forall left. Pool -> Cookies -> Async left Response
viewAll pool cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure player is signed in.
    cookieInfo <- ensureSignedIn pool cookies

    -- Load player conversations.
    loadConversations pool cookieInfo
