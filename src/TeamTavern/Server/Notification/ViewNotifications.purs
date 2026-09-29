module TeamTavern.Server.Notification.ViewNotifications (viewNotifications) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Notification.ViewNotifications as ViewNotifications
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Notification.Infrastructure.Visible (visibleNotification)

-- The list scrolls rather than pages (brief 11.3), so it holds the newest 50.
notificationsQuery :: Query
notificationsQuery = Query $ """
    select
        notification.id,
        notification.kind,
        to_jsonb(notification.created) as created,
        notification.read,
        jsonb_build_object(
            'id', post.id,
            'type', post.ilk,
            'name', post.name,
            'owner', owner.nickname,
            'handle', game.handle,
            'game', game.title,
            'expires', post.updated + case when post.ilk = 'community'
                then interval '90 days' else interval '30 days' end
        ) as post,
        case when fitting.id is not null then jsonb_build_object(
            'id', fitting.id,
            'type', fitting.ilk,
            'name', fitting.name,
            'owner', fitter.nickname
        ) end as fitting
    from notification
    join post on post.id = notification.post_id
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    left join post fitting on fitting.id = notification.fitting_post_id
    left join player fitter on fitter.id = fitting.player_id
    where post.player_id = $1 and """ <> visibleNotification <> """
    order by notification.created desc, notification.id desc
    limit 50
    """

viewNotifications :: ∀ left. Pool -> Cookies -> Async left _
viewNotifications pool cookies =
    sendResponse "Error viewing notifications" do
    { id } <- ensureSignedIn pool cookies
    notifications :: ViewNotifications.OkContent <- queryMany pool notificationsQuery (unwrap id : [])
    pure $ ok_ notifications
