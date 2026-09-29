module TeamTavern.Server.Conversation.Infrastructure.Unread (unreadFor) where

import Prelude

-- | Whether the other side of a `conversation` about a `post` has written since
-- | `reader`, an SQL expression for one of the two sides, last read it: SQL
-- | that is true or false. A read mark can't say who wrote last, so it asks
-- | `message`.
unreadFor :: String -> String
unreadFor reader = """
    exists (
        select from message
        where message.conversation_id = conversation.id
            and message.sender_id <> """ <> reader <> """
            and message.created > coalesce(
                case when post.player_id = """ <> reader <> """
                    then conversation.owner_read_at else conversation.messager_read_at end,
                '-infinity')
    )
    """
