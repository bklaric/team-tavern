module TeamTavern.Server.Conversation.View.LoadConversation
    (LoadConversationResult, LoadConversationError, loadConversation) where

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
import Postgres.Query (Query(..), (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)

type LoadConversationResult = Array
    { nickname :: String
    , content :: String
    , created :: String
    }

type LoadConversationError errors = Variant
    ( unreadableResult ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , databaseError :: Error
    | errors )

queryString :: Query
queryString = Query """
    select
        interlocutor.nickname,
        message.content,
        message.created::text
    from conversation
        join player as left_interlocutor on left_interlocutor.id = conversation.left_interlocutor_id
        join player as right_interlocutor on right_interlocutor.id = conversation.right_interlocutor_id
        join message on message.conversation_id = conversation.id
        join player as interlocutor on interlocutor.id = message.interlocutor_id
    where (conversation.left_interlocutor_id = $1 and right_interlocutor.nickname = $2)
        or (conversation.right_interlocutor_id = $1 and left_interlocutor.nickname = $2)
    order by message.created
    """

loadConversation
    :: forall errors
    .  Pool
    -> Nickname
    -> CookieInfo
    -> Async (LoadConversationError errors) LoadConversationResult
loadConversation pool nickname { id } = do
    result <- pool
        # query queryString (id :| nickname)
        # label (SProxy :: SProxy "databaseError")
    conversation :: LoadConversationResult <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableResult") { result, errors: _ }
    pure conversation
