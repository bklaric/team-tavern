module TeamTavern.Server.Post.Infrastructure.OwnerColumns (ownerView) where

import Prelude

import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Conversation.Infrastructure.Unread (unreadFor)

-- | What a post's owner reads under its facts (brief 11.2), as a JSON object
-- | that decodes into `OwnerView`: SQL over a `post`. A conversation is unread
-- | when the other side has written since the owner last read it, and the one
-- | the count opens comes first in the inbox's order among the unread, or
-- | among all where none is (brief 11.2). A conversation a block hides counts
-- | for nothing, as the inbox doesn't list it.
ownerView :: String
ownerView = """
    jsonb_build_object(
        'expires', post.updated + case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end,
        'conversations', (
            select count(*)
            from conversation
            where conversation.post_id = post.id and """ <> visible <> """
        ),
        'unread', (
            select count(*)
            from conversation
            where conversation.post_id = post.id and """ <> visible <> """
                and """ <> unreadFor "post.player_id" <> """
        ),
        'conversation', (
            select conversation.id
            from conversation
            cross join lateral (
                select max(message.created) as created
                from message
                where message.conversation_id = conversation.id
            ) last
            where conversation.post_id = post.id and """ <> visible <> """
            order by """ <> unreadFor "post.player_id" <> """ desc, last.created desc, conversation.id desc
            limit 1
        ),
        'reveals', post.contact_reveals
    )
    """
    where
    visible = "not " <> blockedBetween "post.player_id" "conversation.messager_id"
