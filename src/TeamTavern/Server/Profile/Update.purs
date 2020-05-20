module TeamTavern.Server.Profile.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfile)
import TeamTavern.Server.Profile.Routes (Identifiers)
import TeamTavern.Server.Profile.Update.LogError (logError)
import TeamTavern.Server.Profile.Update.SendResponse (sendResponse)
import TeamTavern.Server.Profile.Update.UpdateProfile (updateProfile)

update :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
update pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read cookie info from cookies.
    cookieInfo <- readCookieInfo cookies

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load game fields from database.
            fields <- loadFields client identifiers.handle

            -- Read profile from body.
            profile <- readProfile body

            -- Validate profile.
            profile' <- validateProfile fields profile

            -- Update profile.
            updateProfile client cookieInfo identifiers profile'
