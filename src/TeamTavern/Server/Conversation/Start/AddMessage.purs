module TeamTavern.Server.Conversation.Start.AddMessage
    (AddMessageError, addMessage) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Postgres.Async.Query (execute)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Conversation.Start.CreateConversation (ConversationId)
import TeamTavern.Server.Conversation.Start.ValidateMessage (Message)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)

type AddMessageError errors = Variant
    ( databaseError :: Error
    | errors )

queryString :: Query
queryString = Query """
    insert into message (conversation_id, interlocutor_id, content)
    values ($1, $2, $3)
    """

addMessage :: forall querier errors. Querier querier =>
    querier -> CookieInfo -> ConversationId -> Message -> Async (AddMessageError errors) Unit
addMessage querier { id } conversationId message = do
    void $ querier
        # execute queryString (conversationId : id :| message)
        # label (SProxy :: SProxy "databaseError")
