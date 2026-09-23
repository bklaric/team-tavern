module TeamTavern.Server.Block.ReportConversation (reportConversation) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Routes.Shared.Report (Report)
import TeamTavern.Server.Block.Infrastructure.AddReport (Reported, addReport, validateReport)
import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)
import TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail, sendReportEmail)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- The report is against the other side, about the post the conversation is
-- about, whichever side of it the post's owner is.
reportedQuery :: Query
reportedQuery = Query $ """
    select
        reporter.nickname as reporter,
        reported.id as reported_id,
        reported.nickname as reported,
        post.id as post_id,
        game.handle,
        game.title as game,
        post.ilk as type,
        post.name,
        owner.nickname as owner
    from conversation
    join post on post.id = conversation.post_id
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    join player reporter on reporter.id = $2
    join player reported on reported.id =
        case when post.player_id = $2 then conversation.messager_id else post.player_id end
    where conversation.id = $1
        and $2 in (post.player_id, conversation.messager_id)
        and not """ <> blockedBetween "post.player_id" "conversation.messager_id" <> """
    """

reportConversation :: ∀ left. Deployment -> AdminEmail -> Pool -> Int -> Cookies -> Report -> Async left _
reportConversation deployment adminEmail pool conversationId cookies body =
    sendResponse "Error reporting conversation" do
    { id } <- ensureSignedIn pool cookies
    report <- validateReport body
    let viewer = unwrap id
    reported <- pool # transaction \client -> do
        reported :: Reported <- queryFirstNotFound client reportedQuery (conversationId :| viewer)
            # lmap (elaborate ("Can't find conversation " <> show conversationId <> " to report"))
        addReport client viewer reported report
        pure reported
    sendReportEmail deployment adminEmail reported report
    pure noContent_
