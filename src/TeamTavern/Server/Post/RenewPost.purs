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

renewQuery :: Query
renewQuery = Query """
    update post
    set updated = now()
    from game
    where game.id = post.game_id and game.handle = $1 and post.id = $2 and post.player_id = $3
    returning post.id
    """

renewPost :: ∀ left. Pool -> String -> Int -> Cookies -> Async left _
renewPost pool handle postId cookies =
    sendResponse "Error renewing post" do
    { id } <- ensureSignedIn pool cookies
    pool # transaction \client -> do
        (_ :: { id :: Int }) <- queryFirstNotFound client renewQuery (handle : postId :| unwrap id)
            # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to renew"))
        clearExpiry client postId
    pure noContent_
