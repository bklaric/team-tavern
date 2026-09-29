module TeamTavern.Server.Notification.ReadNotification (readNotification) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (elaborate)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstNotFound)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

readQuery :: Query
readQuery = Query """
    update notification
    set read = true
    from post
    where post.id = notification.post_id and post.player_id = $1 and notification.id = $2
    returning notification.id
    """

readNotification :: ∀ left. Pool -> Int -> Cookies -> Async left _
readNotification pool notificationId cookies =
    sendResponse "Error reading notification" do
    { id } <- ensureSignedIn pool cookies
    (_ :: { id :: Int }) <- queryFirstNotFound pool readQuery (unwrap id :| notificationId)
        # lmap (elaborate ("Can't find notification " <> show notificationId <> " to read"))
    pure noContent_
