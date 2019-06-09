module TeamTavern.Server.Profile.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Variant (SProxy(..), inj)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Async.Pool (withTransaction)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Infrastructure.LoadFields (loadFields)
import TeamTavern.Server.Profile.Infrastructure.ReadProfile (readProfile)
import TeamTavern.Server.Profile.Update.LogError (logError)
import TeamTavern.Server.Profile.Update.SendResponse (sendResponse)
import TeamTavern.Server.Profile.Update.UpdateProfile (updateProfile)

update
    :: forall left
    .  Pool
    -> { handle :: Handle, nickname :: Nickname }
    -> Map String String
    -> Body
    -> Async left Response
update pool identifiers cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read cookie info from cookies.
    cookieInfo <- readCookieInfo cookies

    pool # withTransaction (inj (SProxy :: SProxy "databaseError"))
        \client -> do
            -- Load game fields from database.
            fields <- loadFields client identifiers.handle

            -- Read profile from body.
            profile <- readProfile fields body

            -- Update profile.
            updateProfile client cookieInfo identifiers profile
