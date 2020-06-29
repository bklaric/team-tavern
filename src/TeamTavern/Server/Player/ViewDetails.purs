module TeamTavern.Server.Player.ViewDetails where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Newtype (unwrap)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.ViewDetails.LoadDetails (loadDetails)
import TeamTavern.Server.Player.ViewDetails.LogError (logError)
import TeamTavern.Server.Player.ViewDetails.SendResponse (sendResponse)
import TeamTavern.Server.Profile.Routes (Timezone)

viewDetails :: forall left.
    Pool -> Nickname -> Timezone -> Cookies -> Async left Response
viewDetails pool nickname timezone cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure player is signed in as requested nickname.
    cookieInfo <- checkSignedIn pool cookies

    -- Load account.
    loadDetails pool (unwrap nickname) timezone cookieInfo
