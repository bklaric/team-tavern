module TeamTavern.Server.Conversation.ViewAll.LoadConversations
    (LoadConversationsResult, LoadConversationsError, loadConversations) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label, labelMap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)

type LoadConversationsResult = Array
    { nickname :: String
    , unreadMessagesCount :: Int
    , lastMessageCreated :: String
    }

type LoadConversationsError errors = Variant
    ( unreadableResult ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    | errors )

queryString :: Query
queryString = Query """
    select
        player.nickname,
        (count(*) filter (where
            not message.read
            and message.interlocutor_id = conversation.right_interlocutor_id
        ))::integer as "unreadMessagesCount",
        max(message.created)::text as "lastMessageCreated"
    from conversation
        join player on player.id = conversation.right_interlocutor_id
        join message on message.conversation_id = conversation.id
    where conversation.left_interlocutor_id = $1
    group by player.id
    union
    select
        player.nickname,
        (count(*) filter (where
            not message.read
            and message.interlocutor_id = conversation.left_interlocutor_id
        ))::integer as "unreadMessagesCount",
        max(message.created)::text as "lastMessageCreated"
    from conversation
        join player on player.id = conversation.left_interlocutor_id
        join message on message.conversation_id = conversation.id
    where conversation.right_interlocutor_id = $1
    group by player.id
    """

loadConversations
    :: forall errors
    .  Pool
    -> CookieInfo
    -> Async (LoadConversationsError errors) LoadConversationsResult
loadConversations pool { id } = do
    result <- pool
        # query queryString (id : [])
        # label (SProxy :: SProxy "databaseError")
    conversations :: LoadConversationsResult <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableResult") { result, errors: _ }
    pure conversations
