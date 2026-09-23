module TeamTavern.Server.Block.Unblock (unblock) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:|))
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

unblockQuery :: Query
unblockQuery = Query """
    delete from block
    using player
    where block.blocker_id = $1 and block.blocked_id = player.id and lower(player.nickname) = lower($2)
    """

unblock :: ∀ left. Pool -> String -> Cookies -> Async left _
unblock pool nickname cookies =
    sendResponse "Error unblocking player" do
    { id } <- ensureSignedIn pool cookies
    queryNone pool unblockQuery (unwrap id :| nickname)
    pure noContent_
