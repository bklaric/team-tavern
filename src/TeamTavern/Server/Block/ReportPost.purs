module TeamTavern.Server.Block.ReportPost (reportPost) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Report (Report)
import TeamTavern.Server.Block.Infrastructure.AddReport (Reported, addReport, validateReport)
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail, sendReportEmail)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- The posts a contact panel opens on: not the viewer's own, and not one
-- hidden by a block.
reportedQuery :: Query
reportedQuery = Query $ """
    select
        reporter.nickname as reporter,
        owner.id as reported_id,
        owner.nickname as reported,
        post.id as post_id,
        game.handle,
        game.title as game,
        post.ilk as type,
        post.name,
        owner.nickname as owner
    from game
    join post on post.game_id = game.id
    join player owner on owner.id = post.player_id
    join player reporter on reporter.id = $3
    where game.handle = $1 and post.id = $2 and post.player_id <> $3
        and not """ <> blockedBetween "$3" "post.player_id" <> """
    """

reportPost :: ∀ left. Mailer -> AdminEmail -> Pool -> String -> Int -> Cookies -> Report -> Async left _
reportPost mailer adminEmail pool handle postId cookies body =
    sendResponse "Error reporting post" do
    { id } <- ensureSignedIn pool cookies
    report <- validateReport body
    let viewer = unwrap id
    reported <- pool # transaction \client -> do
        reported :: Reported <- queryFirstNotFound client reportedQuery (handle : postId :| viewer)
            # lmap (elaborate ("Can't find post " <> show postId <> " of game " <> handle <> " to report"))
        addReport client viewer reported report
        pure reported
    sendReportEmail mailer adminEmail reported report
    pure noContent_
