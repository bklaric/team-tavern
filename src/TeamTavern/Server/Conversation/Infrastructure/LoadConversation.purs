module TeamTavern.Server.Conversation.Infrastructure.LoadConversation (loadConversation, markRead) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Foreign (Foreign)
import Jarilo (internal__)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Conversation (Conversation, Message)
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, queryMany, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Post.Infrastructure.CardColumns (cardColumns)
import Yoga.JSON.Async (read)

-- Only a side of the conversation finds it, and not after a block either way.
conversationQuery :: Query
conversationQuery = Query $ """
    with parameters as (
        select $2::integer as viewer, now() as now
    )
    select
        """ <> cardColumns <> """,
        '{}'::jsonb as marks,
        game.id as game_id,
        game.handle as game_handle,
        game.title as game_title,
        messager.id as messager_id,
        case when post.player_id = parameters.viewer
            then messager.nickname else owner.nickname end as other,
        to_jsonb(case when post.player_id = parameters.viewer
            then conversation.owner_read_at else conversation.messager_read_at end) as read_to
    from parameters
    join conversation on conversation.id = $1
    join post on post.id = conversation.post_id
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    join player messager on messager.id = conversation.messager_id
    where parameters.viewer in (post.player_id, conversation.messager_id)
        and not """ <> blockedBetween "post.player_id" "conversation.messager_id" <> """
    """

-- The columns the query adds to the card's.
type Extras =
    { game_id :: Int
    , game_handle :: String
    , game_title :: String
    , messager_id :: Int
    , other :: String
    , read_to :: Maybe String
    }

-- The other player's post in the game, player first, as the owner's side of
-- the conversation is headed with it (brief 10, Inbox).
otherPostQuery :: Query
otherPostQuery = Query $ """
    with parameters as (
        select $1::integer as viewer, now() as now
    )
    select
        """ <> cardColumns <> """,
        '{}'::jsonb as marks
    from parameters
    join post on post.player_id = $2 and post.game_id = $3
    join player owner on owner.id = post.player_id
    order by array_position(array['player', 'group', 'community'], post.ilk)
    limit 1
    """

messagesQuery :: Query
messagesQuery = Query """
    select
        message.sender_id = $2 as mine,
        message.content,
        to_jsonb(message.created) as created
    from message
    where message.conversation_id = $1
    order by message.created, message.id
    """

markReadQuery :: Query
markReadQuery = Query """
    update conversation
    set owner_read_at = case when post.player_id = $2
            then now() else conversation.owner_read_at end,
        messager_read_at = case when conversation.messager_id = $2
            then now() else conversation.messager_read_at end
    from post
    where post.id = conversation.post_id and conversation.id = $1
    """

-- | The conversation as `viewer` reads it, or nothing where they aren't one of
-- | its sides. It reads where they had read to, so marking it read comes after.
loadConversation :: ∀ querier errors. Querier querier =>
    querier -> Int -> Int -> Async (InternalTerror_ errors) (Maybe Conversation)
loadConversation querier id viewer = do
    row' :: Maybe Foreign <- queryFirstMaybe querier conversationQuery (id :| viewer)
    for row' \row -> do
        post :: CardRow <- read row # lmap readError
        extras :: Extras <- read row # lmap readError
        otherPost :: Maybe CardRow <-
            if post.own
            then queryFirstMaybe querier otherPostQuery (viewer : extras.messager_id :| extras.game_id)
            else pure Nothing
        messages :: Array Message <- queryMany querier messagesQuery (id :| viewer)
        pure
            { id
            , game: { handle: extras.game_handle, title: extras.game_title }
            , post
            , other: extras.other
            , otherPost
            , readTo: extras.read_to
            , messages
            }
    where
    readError errors = Terror internal__ [ "Error reading conversation row: " <> show errors ]

-- | Marks the conversation read to now on `viewer`'s side.
markRead :: ∀ querier errors. Querier querier => querier -> Int -> Int -> Async (InternalTerror_ errors) Unit
markRead querier id viewer = queryNone querier markReadQuery (id :| viewer)
