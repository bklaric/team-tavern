module TeamTavern.Server.Conversation.ViewInbox (viewInbox) where

import Prelude

import Async (Async)
import Data.Array (filter, nubByEq, partition)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Conversation.ViewInbox (InboxRow)
import TeamTavern.Routes.Conversation.ViewInbox as ViewInbox
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Conversation.Infrastructure.Unread (unreadFor)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- Every conversation the viewer is a side of, latest message first, but those
-- a block hides from both sides.
inboxQuery :: Query
inboxQuery = Query $ """
    with parameters as (
        select $1::integer as viewer, now() as now
    ),
    sides as (
        select conversation.id
        from parameters
        join post on post.player_id = parameters.viewer
        join conversation on conversation.post_id = post.id
        union all
        select conversation.id
        from parameters
        join conversation on conversation.messager_id = parameters.viewer
    )
    select
        conversation.id,
        post.player_id = parameters.viewer as own,
        jsonb_build_object(
            'id', post.id,
            'type', post.ilk,
            'name', post.name,
            'owner', owner.nickname,
            'handle', game.handle,
            'game', game.title,
            'expired', post.updated <= parameters.now - case when post.ilk = 'community'
                then interval '90 days' else interval '30 days' end
        ) as post,
        case when post.player_id = parameters.viewer
            then messager.nickname else owner.nickname end as other,
        jsonb_build_object(
            'mine', last.sender_id = parameters.viewer,
            'sender', sender.nickname,
            'content', last.content,
            'created', last.created
        ) as last,
        """ <> unreadFor "parameters.viewer" <> """ as unread
    from parameters
    join sides on true
    join conversation on conversation.id = sides.id
    join post on post.id = conversation.post_id
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    join player messager on messager.id = conversation.messager_id
    cross join lateral (
        select message.sender_id, message.content, message.created
        from message
        where message.conversation_id = conversation.id
        order by message.created desc, message.id desc
        limit 1
    ) last
    join player sender on sender.id = last.sender_id
    where not """ <> blockedBetween "post.player_id" "conversation.messager_id" <> """
    order by last.created desc, conversation.id desc
    """

type Row = { own :: Boolean | InboxRowFields }

type InboxRowFields =
    ( id :: Int
    , post :: ViewInbox.InboxPost
    , other :: String
    , last :: { mine :: Boolean, sender :: String, content :: Array String, created :: String }
    , unread :: Boolean
    )

row :: Row -> InboxRow
row { id, post, other, last, unread } = { id, post, other, last, unread }

-- The own posts come in the order of their latest conversation, since the
-- rows do.
viewInbox :: ∀ left. Pool -> Cookies -> Async left _
viewInbox pool cookies =
    sendResponse "Error viewing inbox" do
    { id } <- ensureSignedIn pool cookies
    rows :: Array Row <- queryMany pool inboxQuery (unwrap id : [])
    let { yes: own, no: messaged } = partition _.own rows
        groups = own # nubByEq (\one other -> one.post.id == other.post.id) <#> \{ post } ->
            { post, conversations: own # filter (\own' -> own'.post.id == post.id) <#> row }
    pure $ ok_ ({ own: groups, messaged: row <$> messaged } :: ViewInbox.OkContent)
