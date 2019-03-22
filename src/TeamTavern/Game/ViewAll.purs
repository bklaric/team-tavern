module TeamTavern.Game.ViewAll where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.ViewAll.LoadGames (loadGames)
import TeamTavern.Game.ViewAll.LogError (logError)
import TeamTavern.Game.ViewAll.SendResponse (sendResponse)

handleViewAll :: forall left. Pool -> Async left Response
handleViewAll pool =
    sendResponse $ examineLeftWithEffect logError $
    -- Load games from database
    loadGames pool
