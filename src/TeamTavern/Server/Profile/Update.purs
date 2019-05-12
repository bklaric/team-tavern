module TeamTavern.Server.Profile.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Infrastructure.ReadSummary (readSummary)
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

    -- Read summary from body.
    summary <- readSummary body

    -- Update profile.
    updateProfile pool cookieInfo identifiers summary
