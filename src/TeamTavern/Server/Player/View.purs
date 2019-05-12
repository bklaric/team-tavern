module TeamTavern.Server.Player.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.View.LoadPlayer (loadPlayer)
import TeamTavern.Server.Player.View.LogError (logError)
import TeamTavern.Server.Player.View.SendResponse (response)

view :: forall left. Pool -> Nickname -> Async left Response
view pool nickname =
    response $ examineLeftWithEffect logError do
    -- Load player from database.
    loadPlayer pool nickname
