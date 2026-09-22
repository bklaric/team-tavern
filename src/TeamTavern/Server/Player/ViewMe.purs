module TeamTavern.Server.Player.ViewMe (viewMe) where

import Prelude

import Async (Async)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Player.ViewMe as ViewMe
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal, queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

nicknameQuery :: Query
nicknameQuery = Query """
    select player.nickname
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

-- The site has no messages or notifications to count, so both counts are zero.
viewMe :: ∀ left. Pool -> Cookies -> Async left _
viewMe pool cookies =
    sendResponse "Error viewing the signed-in player" do
    {id} <- ensureSignedIn pool cookies
    {nickname} :: {nickname :: String} <- queryFirstInternal pool nicknameQuery (id : [])
    games :: Array ViewMe.OkGameContent <- queryMany pool gamesQuery (id : [])
    pure $ ok_ ({nickname, unreadConversations: 0, unreadNotifications: 0, games} :: ViewMe.OkContent)
