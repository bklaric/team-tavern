module TeamTavern.Server.Player.UpdatePlayer where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Data.Newtype (unwrap)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.UpdatePlayer.LogError (logError)
import TeamTavern.Server.Player.UpdatePlayer.ReadPlayer (readPlayer)
import TeamTavern.Server.Player.UpdatePlayer.SendResponse (sendResponse)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails) as UpdateDetails
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayer)

updatePlayer :: forall left.
    Pool -> Nickname -> Map String String -> Body -> Async left Response
updatePlayer pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies (unwrap nickname)

    -- Read player from body.
    playerModel <- readPlayer body

    -- Validate player.
    player <- validatePlayer playerModel

    -- Update player.
    UpdateDetails.updateDetails pool (unwrap cookieInfo.id) player
