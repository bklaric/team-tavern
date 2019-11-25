module TeamTavern.Server.Conversation.Start.CreateConversation
    (ConversationId, CreateConversationError, createConversation) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor.Label (label, labelMap)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)

type ConversationId = Int

type CreateConversationResult = { conversationId :: Int }

type CreateConversationError errors = Variant
    ( databaseError :: Error
    , unreadableConversationId :: { result :: Result, errors :: MultipleErrors }
    , nothingInsertedSomehow :: { result :: Result }
    | errors )

loadConversationIdString :: Query
loadConversationIdString = Query """
    select conversation.id as "conversationId"
    from conversation
        join player as left_interlocutor on left_interlocutor.id = conversation.left_interlocutor_id
        join player as right_interlocutor on right_interlocutor.id = conversation.right_interlocutor_id
    where (conversation.left_interlocutor_id = $1 and right_interlocutor.nickname = $2)
        or (conversation.right_interlocutor_id = $1 and left_interlocutor.nickname = $2)
    """

insertConversationString :: Query
insertConversationString = Query """
    insert into conversation (left_interlocutor_id, right_interlocutor_id)
    values ($1, (select player.id from player where player.nickname = $2))
    returning conversation.id as "conversationId"
    """

createConversation :: forall querier errors. Querier querier =>
    querier -> Nickname -> CookieInfo -> Async (CreateConversationError errors) ConversationId
createConversation querier nickname { id } = do
    -- Load conversation id from database.
    loadResult <- querier
        # query loadConversationIdString (id :| nickname)
        # label (SProxy :: SProxy "databaseError")
    conversations :: Array CreateConversationResult <- rows loadResult
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableConversationId") { result: loadResult, errors: _ }
    case head conversations of
        -- If conversation actually exists, return its id.
        Just { conversationId } -> pure conversationId
        -- Otherwise create the conversation and return its id.
        Nothing -> do
            insertResult <- querier
                # query insertConversationString (id :| nickname)
                # label (SProxy :: SProxy "databaseError")
            conversations' :: Array CreateConversationResult <- rows insertResult
                # traverse read
                # labelMap (SProxy :: SProxy "unreadableConversationId") { result: insertResult, errors: _ }
            { conversationId } <- head conversations'
                # note (inj (SProxy :: SProxy "nothingInsertedSomehow") { result: insertResult })
            pure conversationId
