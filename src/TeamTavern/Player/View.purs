module TeamTavern.Player.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Player.Infrastructure.ReadNickname (readNickname)
import TeamTavern.Player.View.LoadPlayer (loadPlayer)
import TeamTavern.Player.View.LogError (logError)
import TeamTavern.Player.View.Response (response)

view :: forall left. Pool -> String -> Async left Response
view pool nickname' =
    response $ examineLeftWithEffect logError do
    -- Read player nickname from route.
    nickname <- readNickname nickname'

    -- Load player from database.
    loadPlayer pool nickname
