module TeamTavern.Server.Profile.UpdateTeamProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (validateProfile)
import TeamTavern.Server.Profile.UpdateTeamProfile.LogError (logError)
import TeamTavern.Server.Profile.UpdateTeamProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.UpdateTeamProfile.UpdateProfile (updateProfile)

updateTeamProfile :: forall left.
    Pool -> Map String String -> Body -> { teamHandle :: String, gameHandle :: String } -> Async left Response
updateTeamProfile pool cookies body { teamHandle, gameHandle } =
    sendResponse $ examineLeftWithEffect logError do
    -- Read info info from cookies.
    cookieInfo <- ensureSignedIn pool cookies

    pool # transaction \client -> do
        -- Load game fields from database.
        game <- loadFields client gameHandle

        -- Read profile from body.
        profile <- readProfile body

        -- Validate profile.
        profile' <- validateProfile game profile

        -- Add profile to database.
        updateProfile client cookieInfo teamHandle gameHandle profile'
