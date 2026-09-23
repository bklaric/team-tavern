module TeamTavern.Server.Player.ViewMe (viewMe) where

import Prelude

import Async (Async)
import Jarilo (ok)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Player.ViewMe as ViewMe
import TeamTavern.Server.Conversation.Infrastructure.Unread (unreadFor)
import TeamTavern.Server.Infrastructure.Cookie (Cookies, setCookieHeader)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- The conversations about the player's posts and those they started, each
-- counted once however much is unread in it (brief 11.4).
playerQuery :: Query
playerQuery = Query $ """
    select
        player.nickname,
        (
            select count(*)::int
            from conversation
            join post on post.id = conversation.post_id
            where (post.player_id = player.id or conversation.messager_id = player.id)
                and """ <> unreadFor "player.id" <> """
        ) as unread_conversations
    from player
    where player.id = $1
    """

gamesQuery :: Query
gamesQuery = Query """
    select
        game.handle,
        count(*)::int as posts,
        array_agg(post.ilk order by array_position(array['player', 'group', 'community'], post.ilk)) as types
    from post
        join game on game.id = post.game_id
    where post.player_id = $1
    group by game.handle, game.title
    order by game.title
    """

-- The site has no notifications to count yet, so that count is zero.
-- The header asks on every page, so the answer renews the session cookie,
-- which then lapses when the session does.
viewMe :: ∀ left. Deployment -> Pool -> Cookies -> Async left _
viewMe deployment pool cookies =
    sendResponse "Error viewing the signed-in player" do
    {id, token} <- ensureSignedIn pool cookies
    {nickname, unread_conversations} :: {nickname :: String, unread_conversations :: Int}
        <- queryFirstInternal pool playerQuery (id : [])
    games :: Array ViewMe.OkGameContent <- queryMany pool gamesQuery (id : [])
    pure $ ok (setCookieHeader deployment token)
        ({nickname, unreadConversations: unread_conversations, unreadNotifications: 0, games} :: ViewMe.OkContent)
