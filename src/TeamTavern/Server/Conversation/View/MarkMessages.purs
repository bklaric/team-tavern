module TeamTavern.Server.Conversation.View.MarkMessages
    (MarkMessagesError, markMessages) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Postgres.Async.Query (execute)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), (:|))
import TeamTavern.Server.Conversation.Routes (Nickname)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)

type MarkMessagesError errors = Variant
    ( databaseError :: Error
    | errors )

queryString :: Query
queryString = Query """
    update message
    set read = true
    from
        conversation,
        player as left_interlocutor,
        player as right_interlocutor,
        player as interlocutor
    where
        -- Join conversation and messages.
        conversation.id = message.conversation_id
        -- Join conversation interlocutors.
        and left_interlocutor.id = conversation.left_interlocutor_id
        and right_interlocutor.id = conversation.right_interlocutor_id
        -- Join message interlocutor.
        and interlocutor.id = message.interlocutor_id
        -- Find correct conversation.
        and (
            (conversation.left_interlocutor_id = $1 and right_interlocutor.nickname = $2)
            or (conversation.right_interlocutor_id = $1 and left_interlocutor.nickname = $2)
        )
        -- Find messages sent by the other interlocutor, not the one loading the conversation.
        and interlocutor.nickname = $2
    """

markMessages :: forall errors.
    Client -> Nickname -> CookieInfo -> Async (MarkMessagesError errors) Unit
markMessages client nickname { id } =
    client
        # execute queryString (id :| nickname)
        # label (SProxy :: SProxy "databaseError")
