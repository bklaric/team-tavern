module TeamTavern.Server.Player.UpdatePlayer where

import Prelude

import Async (Async)
import Data.Map (Map)
import Data.Newtype (unwrap)
import Jarilo (noContent_)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.UpdatePlayer as UpdatePlayer
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.UpdatePlayer.UpdateDetails (updateDetails) as UpdateDetails
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (validatePlayer)

updatePlayer :: ∀ left.
    Pool -> Nickname -> Map String String -> UpdatePlayer.RequestContent -> Async left _
updatePlayer pool nickname cookies player' =
    sendResponse "Error updating player" do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies (unwrap nickname)

    -- Validate player.
    player <- validatePlayer player'

    -- Update player.
    UpdateDetails.updateDetails pool (unwrap cookieInfo.id) player

    pure noContent_
