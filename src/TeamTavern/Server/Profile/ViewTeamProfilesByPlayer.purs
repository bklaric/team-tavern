module TeamTavern.Server.Profile.ViewTeamProfilesByPlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Nickname, Timezone)
import TeamTavern.Server.Profile.ViewTeamProfilesByPlayer.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewTeamProfilesByPlayer.LogError (logError)
import TeamTavern.Server.Profile.ViewTeamProfilesByPlayer.SendResponse (sendResponse)

viewTeamProfilesByPlayer :: forall left.
    Pool -> Nickname -> Timezone -> Async left Response
viewTeamProfilesByPlayer pool nickname timezone =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool nickname timezone
