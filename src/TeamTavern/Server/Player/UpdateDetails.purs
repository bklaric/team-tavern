module TeamTavern.Server.Player.UpdateDetails where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Newtype (unwrap)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.UpdateDetails.LogError (logError)
import TeamTavern.Server.Player.UpdateDetails.ReadUpdate (readUpdate)
import TeamTavern.Server.Player.UpdateDetails.SendResponse (sendResponse)
import TeamTavern.Server.Player.UpdateDetails.UpdateDetails (updateDetails) as UpdateDetails

updateDetails :: forall left.
    Pool -> Nickname -> Map String String -> Body -> Async left Response
updateDetails pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies (unwrap nickname)

    -- Read update from body.
    update' <- readUpdate body

    -- Update player.
    UpdateDetails.updateDetails pool (unwrap cookieInfo.id) update'
