module TeamTavern.Profile.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Profile.Infrastructure.ReadIdentifiers (readIdentifiers)
import TeamTavern.Profile.Infrastructure.Types (IdentifiersModel)
import TeamTavern.Profile.View.LoadProfile (loadProfile)
import TeamTavern.Profile.View.LogError (logError)
import TeamTavern.Profile.View.Response (response)

view :: forall left. Pool -> IdentifiersModel -> Async left Response
view pool identifiers' = response $ examineLeftWithEffect logError do
    -- Read profile identifiers from route.
    identifiers <- readIdentifiers identifiers'

    -- Load profile from database.
    loadProfile pool identifiers
