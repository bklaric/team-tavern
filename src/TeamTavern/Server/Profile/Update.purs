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
import TeamTavern.Server.Profile.Infrastructure.LoadFields (loadFields)
import TeamTavern.Server.Profile.Infrastructure.ReadProfile (readProfile)
import TeamTavern.Server.Profile.Routes (Identifiers)
import TeamTavern.Server.Profile.Update.LogError (logError)
import TeamTavern.Server.Profile.Update.SendResponse (sendResponse)
import TeamTavern.Server.Profile.Update.UpdateProfile (updateProfile)
import Unsafe.Coerce (unsafeCoerce)

update :: forall left.
    Pool -> Identifiers -> Map String String -> Body -> Async left Response
update pool identifiers cookies body =
    sendResponse do
    -- Read cookie info from cookies.
    cookieInfo <- readCookieInfo cookies

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load game fields from database.
            fields <- loadFields client (unsafeCoerce identifiers.handle)

            -- Read profile from body.
            profile <- readProfile body

            -- Update profile.
            updateProfile client cookieInfo (unsafeCoerce identifiers) profile
