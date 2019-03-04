module TeamTavern.Profile.ViewAll where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Profile.Routes (IdentifiersMany)
import TeamTavern.Profile.ViewAll.LoadProfiles (loadProfiles)
import TeamTavern.Profile.ViewAll.LogError (logError)
import TeamTavern.Profile.ViewAll.SendResponse (sendResponse)

viewAll :: forall left. Pool -> IdentifiersMany -> Async left Response
viewAll pool identifiers =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profiles.
    loadProfiles pool identifiers
