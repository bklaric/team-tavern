module TeamTavern.Server.Profile.AddGamePlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Server.Profile.AddGamePlayer.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddGamePlayer.LogError (logError)
import TeamTavern.Server.Profile.AddGamePlayer.SendResponse (sendResponse)
import TeamTavern.Server.Profile.Infrastructure.LoadFields (loadFields)
import TeamTavern.Server.Profile.Infrastructure.ReadProfile (readProfile)
import TeamTavern.Server.Profile.Infrastructure.ValidateProfile (validateProfile)
import TeamTavern.Server.Profile.Routes (Identifiers)

addGamePlayer :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
addGamePlayer pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read info info from cookies.
    cookieInfo <- readCookieInfo cookies

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load game fields from database.
            fields <- loadFields client identifiers.handle

            -- Read profile from body.
            profile <- readProfile body

            -- Validate profile.
            profile' <- validateProfile fields profile

            -- Add profile to database.
            addProfile client cookieInfo identifiers profile'
