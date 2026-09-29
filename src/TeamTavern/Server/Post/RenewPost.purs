module TeamTavern.Server.Post.RenewPost (renewPost) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query ((:), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.Renew (renew)

renewPost :: ∀ left. Pool -> String -> Int -> Cookies -> Async left _
renewPost pool handle postId cookies =
    sendResponse "Error renewing post" do
    { id } <- ensureSignedIn pool cookies
    pool # transaction \client ->
        renew client "game.handle = $1 and post.id = $2 and post.player_id = $3" (handle : postId :| unwrap id)
            # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to renew"))
            # void
    pure noContent_
