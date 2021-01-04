module TeamTavern.Server.Game.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.View.LoadGame (loadGame)
import TeamTavern.Server.Game.View.LogError (logError)
import TeamTavern.Server.Game.View.SendResponse (sendResponse)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)

view :: forall left. Pool -> String -> Cookies -> Async left Response
view pool handle cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Load game from database.
    loadGame pool handle
