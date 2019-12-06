module TeamTavern.Server.Conversation.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Variant (SProxy(..), inj)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Conversation.View.LoadConversation (loadConversation)
import TeamTavern.Server.Conversation.View.LogError (logError)
import TeamTavern.Server.Conversation.View.MarkMessages (markMessages)
import TeamTavern.Server.Conversation.View.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)

view :: forall left. Pool -> Nickname -> Cookies -> Async left Response
view pool nickname cookies =
    sendResponse $ examineLeftWithEffect logError do

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Ensure player is signed in.
            cookieInfo <- ensureSignedIn client cookies

            -- Mark messages as read.
            markMessages client nickname cookieInfo

            -- Load specified conversations.
            loadConversation client nickname cookieInfo
