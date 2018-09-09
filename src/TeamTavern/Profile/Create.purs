module TeamTavern.Profile.Create where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Infrastructure.ReadAuth (readAuth)
import TeamTavern.Profile.Create.AddProfile (addProfile)
import TeamTavern.Profile.Create.LogError (logError)
import TeamTavern.Profile.Create.Response (response)
import TeamTavern.Profile.Infrastructure.ReadIdentifiers (readIdentifiers)
import TeamTavern.Profile.Infrastructure.ReadSummary (readSummary)
import TeamTavern.Profile.Infrastructure.Types (IdentifiersModel)

create :: forall left.
    Pool -> IdentifiersModel -> Map String String -> Body -> Async left Response
create pool identifiers' cookies body =
    response $ examineLeftWithEffect logError do
    -- Read identifiers from route.
    identifiers <- readIdentifiers identifiers'

    -- Read auth info from cookies.
    auth <- readAuth cookies

    -- Read summary from body.
    summary <- readSummary body

    -- Add profile to database.
    addProfile pool auth identifiers summary
