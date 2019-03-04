module TeamTavern.Profile.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Profile.Routes (IdentifiersSingle)
import TeamTavern.Profile.View.LoadProfile (loadProfile)
import TeamTavern.Profile.View.LogError (logError)
import TeamTavern.Profile.View.SendResponse (sendResponse)

view :: forall left. Pool -> IdentifiersSingle -> Async left Response
view pool identifiers =
    sendResponse $ examineLeftWithEffect logError do
    -- Load profile from database.
    loadProfile pool identifiers
