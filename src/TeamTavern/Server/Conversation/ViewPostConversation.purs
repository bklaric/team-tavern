module TeamTavern.Server.Conversation.ViewPostConversation (viewPostConversation) where

import Prelude

import Async (Async)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Conversation.ViewPostConversation as ViewPostConversation
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Conversation.Infrastructure.LoadConversation (loadConversation, markRead)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

conversationQuery :: Query
conversationQuery = Query $ """
    select conversation.id
    from game
    join post on post.game_id = game.id
    join conversation on conversation.post_id = post.id
    where game.handle = $1 and post.id = $2 and conversation.messager_id = $3
        and not """ <> blockedBetween "post.player_id" "conversation.messager_id" <> """
    """

viewPostConversation :: ∀ left. Pool -> String -> Int -> Cookies -> Async left _
viewPostConversation pool handle postId cookies =
    sendResponse "Error viewing post conversation" do
    { id } <- ensureSignedIn pool cookies
    let viewer = unwrap id
    found :: Maybe { id :: Int } <- queryFirstMaybe pool conversationQuery (handle : postId :| viewer)
    conversation <- for found \{ id: conversationId } -> loadConversation pool conversationId viewer
    for_ found \{ id: conversationId } -> markRead pool conversationId viewer
    pure $ ok_ ({ conversation: join conversation } :: ViewPostConversation.OkContent)
