module TeamTavern.Server.Player.ViewSettings where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.ViewSettings.LoadSettings (loadSettings)
import TeamTavern.Server.Player.ViewSettings.LogError (logError)
import TeamTavern.Server.Player.ViewSettings.SendResponse (sendResponse)

viewSettings :: forall left. Pool -> String -> Cookies -> Async left Response
viewSettings pool nickname cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Ensure player is signed in as requested nickname.
    cookieInfo <- ensureSignedInAs pool cookies nickname

    -- Load settings.
    loadSettings pool nickname
