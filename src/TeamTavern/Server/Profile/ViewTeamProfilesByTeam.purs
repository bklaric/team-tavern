module TeamTavern.Server.Profile.ViewTeamProfilesByTeam where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Nickname, Timezone)
import TeamTavern.Server.Profile.ViewTeamProfilesByTeam.LoadProfiles (loadProfiles)
import TeamTavern.Server.Profile.ViewTeamProfilesByTeam.LogError (logError)
import TeamTavern.Server.Profile.ViewTeamProfilesByTeam.SendResponse (sendResponse)

viewTeamProfilesByPlayer :: forall left.
    Pool -> Nickname -> Timezone -> Async left Response
viewTeamProfilesByPlayer pool nickname timezone =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool nickname timezone
