module TeamTavern.Server.Post.RenewPost (renewPost) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.ClearExpiry (clearExpiry)
import TeamTavern.Server.Post.Infrastructure.NotifyFits (notifyFits)

-- Whether the post had expired is read before the update, since returning sees
-- only the new time.
renewQuery :: Query
renewQuery = Query """
    update post
    set updated = now()
    from (
        select post.id, post.updated <= now() - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end as expired
        from post
        join game on game.id = post.game_id
        where game.handle = $1 and post.id = $2 and post.player_id = $3
        for update of post
    ) renewed
    where post.id = renewed.id
    returning renewed.expired
    """

-- | Renewing an expired post tells the owners of the posts it fits again, as
-- | publishing it did (brief 8); renewing an active one tells nobody.
renewPost :: ∀ left. Pool -> String -> Int -> Cookies -> Async left _
renewPost pool handle postId cookies =
    sendResponse "Error renewing post" do
    { id } <- ensureSignedIn pool cookies
    pool # transaction \client -> do
        { expired } :: { expired :: Boolean } <- queryFirstNotFound client renewQuery (handle : postId :| unwrap id)
            # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to renew"))
        clearExpiry client postId
        when expired $ notifyFits client postId
    pure noContent_
