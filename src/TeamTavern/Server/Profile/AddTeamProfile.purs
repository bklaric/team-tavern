module TeamTavern.Server.Profile.AddTeamProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddTeamProfile.LogError (logError)
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddTeamProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (validateProfile)

addTeamProfile :: forall left.
    Pool -> Map String String -> Body -> { teamHandle :: String, gameHandle :: String } -> Async left Response
addTeamProfile pool cookies body { teamHandle, gameHandle } =
    sendResponse $ examineLeftWithEffect logError do
    -- Read info from cookies.
    cookieInfo <- ensureSignedIn pool cookies

    pool # transaction \client -> do
        -- Load game fields from database.
        game <- loadFields client gameHandle

        -- Read profile from body.
        profile <- readProfile body

        -- Validate profile.
        profile' <- validateProfile game profile

        -- Add profile to database.
        addProfile client cookieInfo.id teamHandle gameHandle profile'
