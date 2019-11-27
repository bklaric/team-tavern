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
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)

type LoadConversationResult = Array
    { nickname :: String
    , content :: Array String
    , created :: String
    , createdSeconds :: Number
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
        message.created::text,
        extract(epoch from (now() - message.created)) as "createdSeconds"
    from conversation
        join player as left_interlocutor on left_interlocutor.id = conversation.left_interlocutor_id
        join player as right_interlocutor on right_interlocutor.id = conversation.right_interlocutor_id
        join message on message.conversation_id = conversation.id
        join player as interlocutor on interlocutor.id = message.interlocutor_id
    where (conversation.left_interlocutor_id = $1 and lower(right_interlocutor.nickname) = lower($2))
        or (conversation.right_interlocutor_id = $1 and lower(left_interlocutor.nickname) = lower($2))
    order by message.created
    """

loadConversation
    :: forall errors
    .  Client
    -> Nickname
    -> CookieInfo
    -> Async (LoadConversationError errors) LoadConversationResult
loadConversation client nickname { id } = do
    result <- client
        # query queryString (id :| nickname)
        # label (SProxy :: SProxy "databaseError")
    conversation :: LoadConversationResult <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadableResult") { result, errors: _ }
    pure conversation
