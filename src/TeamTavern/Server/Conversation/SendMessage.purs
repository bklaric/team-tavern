module TeamTavern.Server.Conversation.SendMessage (ConversationLimits, createConversationLimits, sendMessage) where

import Prelude

import Async (Async, attempt, foreach, fromEffect, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Variant (Variant)
import Effect (Effect)
import Jarilo (BadRequestRow_, InternalRow_, NotAuthorizedRow_, NotFoundRow_, OkRow, TooManyRequestsRow, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Conversation.SendMessage (TooManyContent)
import TeamTavern.Routes.Shared.Conversation (Conversation, MessageContent)
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Conversation.Infrastructure.PostMessage (postMessage, validateMessage)
import TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (sendMessageEmail)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryFirstMaybe, queryFirstNotFound, transaction)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter, admit, createLimiter, ensureAdmissionWith, release)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Row (type (+))

-- | Each owner is emailed what's written to them, and a new account costs
-- | nothing, so an account starts 3 conversations a day until its email is
-- | confirmed, and 20 once it is. Writing on in one doesn't count.
type ConversationLimits = { unconfirmed :: Limiter, confirmed :: Limiter }

createConversationLimits :: Effect ConversationLimits
createConversationLimits = do
    unconfirmed <- createLimiter "Conversations started by one unconfirmed account" { count: 3, seconds: 86400 }
    confirmed <- createLimiter "Conversations started by one confirmed account" { count: 20, seconds: 86400 }
    pure { unconfirmed, confirmed }

-- Nobody messages their own post, or a post of a player either has blocked.
postCondition :: String
postCondition = """
    game.handle = $1 and post.id = $2 and post.player_id <> $3
        and not """ <> blockedBetween "$3" "post.player_id"

postQuery :: Query
postQuery = Query $ """
    select post.id
    from game
    join post on post.game_id = game.id
    where """ <> postCondition

-- | Whether the message would start a conversation, which counts, and whether
-- | the viewer's email is confirmed, which says how many they start.
startingQuery :: Query
startingQuery = Query $ """
    select
        player.email_confirmed as confirmed,
        exists (
            select 1 from game join post on post.game_id = game.id
            where """ <> postCondition <> """
        )
        and not exists (
            select 1 from conversation where post_id = $2 and messager_id = $3
        ) as starting
    from player
    where player.id = $3
    """

startQuery :: Query
startQuery = Query """
    insert into conversation (post_id, messager_id)
    values ($1, $2)
    on conflict (post_id, messager_id) do nothing
    returning id
    """

conversationQuery :: Query
conversationQuery = Query """
    select id from conversation where post_id = $1 and messager_id = $2
    """

sendMessage :: ∀ left. ConversationLimits -> Mailer -> Pool -> String -> Int -> Cookies -> MessageContent
    -> Async left (Variant (OkRow Conversation + BadRequestRow_ + NotAuthorizedRow_ + NotFoundRow_ + TooManyRequestsRow TooManyContent + InternalRow_ + ()))
sendMessage limits mailer pool handle postId cookies { content } =
    sendResponse "Error sending message" do
    { id } <- ensureSignedIn pool cookies
    lines <- validateMessage content
    let viewer = unwrap id
        account = show viewer

    -- A conversation is counted before it's started, so messages sent at once
    -- can't all find room, and given back if none was. A post that can't be
    -- messaged isn't counted, and the transaction says why.
    { confirmed, starting } :: { confirmed :: Boolean, starting :: Boolean } <-
        queryFirstInternal pool startingQuery (handle : postId :| viewer)
    counted <-
        if not starting then pure Nothing
        else do
            let limiter = if confirmed then limits.confirmed else limits.unconfirmed
            ensureAdmissionWith { seconds: _, confirmed } (admit limiter account)
                "Too many conversations started by one account."
            pure $ Just limiter

    result <- attempt $ pool # transaction \client -> do
        (_ :: { id :: Int }) <- queryFirstNotFound client postQuery (handle : postId :| viewer)
            # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to message"))
        started :: Maybe { id :: Int } <- queryFirstMaybe client startQuery (postId :| viewer)
        { id: conversationId } :: { id :: Int } <- case started of
            Just conversation -> pure conversation
            Nothing -> queryFirstInternal client conversationQuery (postId :| viewer)
        posted <- postMessage client conversationId viewer lines
        pure { posted, started: isJust started }
    let keep = case result of
            Right { started } -> started
            Left _ -> false
    foreach counted \limiter -> unless keep $ fromEffect $ release limiter account
    { posted: { conversation, email } } <- fromEither result

    foreach email $ sendMessageEmail mailer
    pure $ ok_ conversation
