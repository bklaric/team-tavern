module TeamTavern.Server.Block.ViewBlocked (viewBlocked) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Routes.Block.ViewBlocked as ViewBlocked
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryMany)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

blockedQuery :: Query
blockedQuery = Query """
    select player.nickname
    from block
    join player on player.id = block.blocked_id
    where block.blocker_id = $1
    order by lower(player.nickname)
    """

viewBlocked :: ∀ left. Pool -> Cookies -> Async left _
viewBlocked pool cookies =
    sendResponse "Error viewing blocked players" do
    { id } <- ensureSignedIn pool cookies
    blocked :: ViewBlocked.OkContent <- queryMany pool blockedQuery (unwrap id : [])
    pure $ ok_ blocked
