module TeamTavern.Server.Post.Infrastructure.OwnerColumns (ownerView) where

-- | What a post's owner reads under its facts (brief 11.2), as a JSON object
-- | that decodes into `OwnerView`: SQL over a `post`. A conversation is unread
-- | when the other side has written since the owner last read it.
ownerView :: String
ownerView = """
    jsonb_build_object(
        'expires', post.updated + case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end,
        'conversations', (select count(*) from conversation where conversation.post_id = post.id),
        'unread', (
            select count(*)
            from conversation
            where conversation.post_id = post.id and exists (
                select from message
                where message.conversation_id = conversation.id
                    and message.sender_id <> post.player_id
                    and message.created > coalesce(conversation.owner_read_at, '-infinity')
            )
        ),
        'reveals', post.contact_reveals
    )
    """
