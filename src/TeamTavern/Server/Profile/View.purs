module TeamTavern.Server.Profile.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Profile.Routes (Identifiers)
import TeamTavern.Server.Profile.View.LoadProfile (loadProfile)
import TeamTavern.Server.Profile.View.LogError (logError)
import TeamTavern.Server.Profile.View.SendResponse (sendResponse)

view :: forall left. Pool -> Identifiers -> Async left Response
view pool identifiers =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profile from database.
    loadProfile pool identifiers
