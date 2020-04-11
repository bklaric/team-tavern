module TeamTavern.Server.Profile.AddGameTeam where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Profile.AddGameTeam.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddGameTeam.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddGameTeam.LogError (logError)
import TeamTavern.Server.Profile.AddGameTeam.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddGameTeam.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddGameTeam.ValidateProfile (validateProfile)
import TeamTavern.Server.Profile.Routes (Identifiers)

addGameTeam :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
addGameTeam pool identifiers cookies body =
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
