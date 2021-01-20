module TeamTavern.Server.Profile.AddPlayerProfile where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Newtype (unwrap)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Profile.AddPlayerProfile.AddProfile (addProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.LoadFields (loadFields)
import TeamTavern.Server.Profile.AddPlayerProfile.LogError (logError)
import TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile (readProfile)
import TeamTavern.Server.Profile.AddPlayerProfile.SendResponse (sendResponse)
import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (validateProfile)
import TeamTavern.Server.Profile.Routes (Identifiers)

addPlayerProfile :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
addPlayerProfile pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do

    pool # transaction \client -> do
        -- Read info info from cookies.
        cookieInfo <- ensureSignedInAs client cookies identifiers.nickname

        -- Load game fields from database.
        game <- loadFields client identifiers.handle

        -- Read profile from body.
        profile <- readProfile body

        -- Validate profile.
        profile' <- validateProfile game.fields profile

        -- Add profile to database.
        addProfile client (unwrap cookieInfo.id) identifiers profile'
