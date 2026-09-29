module TeamTavern.Server.Account.UpdateSwitches (updateSwitches) where

import Prelude

import Async (Async)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Account.ViewAccount (Switches)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

queryString :: Query
queryString = Query """
    update player
    set email_matches = $2, email_messages = $3, email_renewals = $4
    where id = $1
    """

updateSwitches :: ∀ left. Pool -> Cookies -> Switches -> Async left _
updateSwitches pool cookies { matches, messages, renewals } =
    sendResponse "Error updating email switches" do
    { id } <- ensureSignedIn pool cookies
    queryNone pool queryString (unwrap id : matches : messages :| renewals)
    pure noContent_
