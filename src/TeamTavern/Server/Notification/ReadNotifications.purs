module TeamTavern.Server.Notification.ReadNotifications (readNotifications) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

readQuery :: Query
readQuery = Query """
    update notification
    set read = true
    from post
    where post.id = notification.post_id and post.player_id = $1 and not notification.read
    """

readNotifications :: ∀ left. Pool -> Cookies -> Async left _
readNotifications pool cookies =
    sendResponse "Error reading notifications" do
    { id } <- ensureSignedIn pool cookies
    queryNone pool readQuery (unwrap id : [])
    pure noContent_
