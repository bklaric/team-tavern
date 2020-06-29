module TeamTavern.Server.Profile.AddTeamProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Profile.AddTeamProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddTeamProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddTeamProfile.LogError (logError)
import TeamTavern.Server.Profile.AddTeamProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddTeamProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (validateProfile)
import TeamTavern.Server.Profile.Routes (Identifiers)

addTeamProfile :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
addTeamProfile pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read info info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies identifiers.nickname

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load game fields from database.
            fields <- loadFields client identifiers.handle

            -- Read profile from body.
            profile <- readProfile body

            -- Validate profile.
            profile' <- validateProfile fields profile

            -- Add profile to database.
            addProfile client cookieInfo identifiers.handle profile'
