module TeamTavern.Server.Account.DeleteAccount (deleteAccount) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (noContent)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:))
import TeamTavern.Server.Infrastructure.Cookie (Cookies, removeCookieHeader)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

-- Everything that names the player goes with it by cascade: sessions, nonces,
-- posts with their answers, conversations and notifications, the conversations
-- they started on others' posts, blocks both ways and reports.
deleteQuery :: Query
deleteQuery = Query """
    delete from player where id = $1
    """

deleteAccount :: ∀ left. Pool -> Cookies -> Async left _
deleteAccount pool cookies =
    sendResponse "Error deleting account" do
    { id } <- ensureSignedIn pool cookies
    queryNone pool deleteQuery (unwrap id : [])
    pure $ noContent removeCookieHeader
