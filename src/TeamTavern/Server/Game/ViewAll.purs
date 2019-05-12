module TeamTavern.Server.Game.ViewAll where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.ViewAll.LoadGames (loadGames)
import TeamTavern.Server.Game.ViewAll.LogError (logError)
import TeamTavern.Server.Game.ViewAll.SendResponse (sendResponse)

handleViewAll :: forall left. Pool -> Async left Response
handleViewAll pool =
    sendResponse $ examineLeftWithEffect logError $
    -- Load games from database
    loadGames pool
