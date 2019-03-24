module TeamTavern.Profile.Create where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Profile.Create.AddProfile (addProfile)
import TeamTavern.Profile.Create.LogError (logError)
import TeamTavern.Profile.Create.SendResponse (sendResponse)
import TeamTavern.Profile.Infrastructure.ReadSummary (readSummary)

create :: forall left.
    Pool -> Handle -> Map String String -> Body -> Async left Response
create pool handle cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read info info from cookies.
    cookieInfo <- readCookieInfo cookies

    -- Read summary from body.
    summary <- readSummary body

    -- Add profile to database.
    addProfile pool cookieInfo handle summary
