module TeamTavern.Server.Player.UpdatePlayer.ReadPlayer where

import Async (Async)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Player.UpdatePlayer as UpdatePlayer
import TeamTavern.Server.Infrastructure.Error (ClientError)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)

readPlayer :: forall errors. Body -> Async (ClientError errors) UpdatePlayer.RequestContent
readPlayer = readJsonBody
