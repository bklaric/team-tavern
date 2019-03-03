module TeamTavern.Profile.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Infrastructure.ReadCookieInfo (readAuth)
import TeamTavern.Profile.Infrastructure.ReadIdentifiers (readIdentifiers)
import TeamTavern.Profile.Infrastructure.ReadSummary (readSummary)
import TeamTavern.Profile.Infrastructure.Types (IdentifiersModel)
import TeamTavern.Profile.Update.LogError (logError)
import TeamTavern.Profile.Update.Response (response)
import TeamTavern.Profile.Update.UpdateProfile (updateProfile)

update
    :: forall left
    .  Pool
    -> IdentifiersModel
    -> Map String String
    -> Body
    -> Async left Response
update pool identifiers' cookies body =
    response $ examineLeftWithEffect logError do
    -- Read identifiers from route.
    identifiers <- readIdentifiers identifiers'

    -- Read auth info from cookies.
    auth <- readAuth cookies

    -- Read summary from body.
    summary <- readSummary body

    -- Update profile.
    updateProfile pool auth identifiers summary
