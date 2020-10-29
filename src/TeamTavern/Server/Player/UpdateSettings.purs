module TeamTavern.Server.Player.EditSettings where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.EditSettings.LogError (logError)
import TeamTavern.Server.Player.EditSettings.ReadSettings (readSettings)
import TeamTavern.Server.Player.EditSettings.SendResponse (sendResponse)
import TeamTavern.Server.Player.EditSettings.UpdateSettings as UpdateSettings

updateSettings :: forall left.
    Pool -> String -> Map String String -> Body -> Async left Response
updateSettings pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies nickname

    -- Read settings from body.
    settings <- readSettings body

    -- Update settings.
    UpdateSettings.updateSettings pool cookieInfo settings
