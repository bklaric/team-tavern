module TeamTavern.Server.Player.ViewDetails where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Newtype (unwrap)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.ViewDetails.LoadDetails (loadDetails)
import TeamTavern.Server.Player.ViewDetails.LogError (logError)
import TeamTavern.Server.Player.ViewDetails.SendResponse (sendResponse)

viewDetails :: forall left. Pool -> Nickname -> Cookies -> Async left Response
viewDetails pool nickname cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure player is signed in as requested nickname.
    cookieInfo <- ensureSignedInAs pool cookies (unwrap nickname)

    -- Load account.
    loadDetails pool (unwrap nickname)
