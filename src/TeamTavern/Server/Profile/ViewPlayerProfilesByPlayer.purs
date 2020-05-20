module TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Nickname)
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.LogError (logError)
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.SendResponse (sendResponse)

viewPlayerProfilesByPlayer :: forall left.
    Pool -> Nickname -> Async left Response
viewPlayerProfilesByPlayer pool nickname =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool nickname
