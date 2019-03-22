module TeamTavern.Player.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.View.LoadPlayer (loadPlayer)
import TeamTavern.Player.View.LogError (logError)
import TeamTavern.Player.View.SendResponse (response)

view :: forall left. Pool -> Nickname -> Async left Response
view pool nickname =
    response $ examineLeftWithEffect logError do
    -- Load player from database.
    loadPlayer pool nickname
