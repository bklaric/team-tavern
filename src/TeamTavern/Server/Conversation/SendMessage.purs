module TeamTavern.Server.Conversation.SendMessage (sendMessage) where

import Prelude

import Async (Async, foreach)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Conversation (MessageContent)
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Conversation.Infrastructure.PostMessage (postMessage, validateMessage)
import TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (sendMessageEmail)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryFirstNotFound, queryNone, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- Nobody messages their own post, or a post of a player either has blocked.
postQuery :: Query
postQuery = Query $ """
    select post.id
    from game
    join post on post.game_id = game.id
    where game.handle = $1 and post.id = $2 and post.player_id <> $3
        and not """ <> blockedBetween "$3" "post.player_id" <> """
    """

startQuery :: Query
startQuery = Query """
    insert into conversation (post_id, messager_id)
    values ($1, $2)
    on conflict (post_id, messager_id) do nothing
    """

conversationQuery :: Query
conversationQuery = Query """
    select id from conversation where post_id = $1 and messager_id = $2
    """

sendMessage :: ∀ left. Mailer -> Pool -> String -> Int -> Cookies -> MessageContent -> Async left _
sendMessage mailer pool handle postId cookies { content } =
    sendResponse "Error sending message" do
    { id } <- ensureSignedIn pool cookies
    lines <- validateMessage content
    let viewer = unwrap id
    { conversation, email } <- pool # transaction \client -> do
        (_ :: { id :: Int }) <- queryFirstNotFound client postQuery (handle : postId :| viewer)
            # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to message"))
        queryNone client startQuery (postId :| viewer)
        { id: conversationId } :: { id :: Int } <- queryFirstInternal client conversationQuery (postId :| viewer)
        postMessage client conversationId viewer lines
    foreach email $ sendMessageEmail mailer
    pure $ ok_ conversation
