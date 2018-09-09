module TeamTavern.Game.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Infrastructure.ReadHandle (readHandle)
import TeamTavern.Game.View.LoadGame (loadGame)
import TeamTavern.Game.View.LogError (logError)
import TeamTavern.Game.View.Response (response)

handleView :: forall left. Pool -> String -> Async left Response
handleView pool handle' =
    response $ examineLeftWithEffect logError do
    -- Validate game handle from route.
    handle <- readHandle handle'

    -- Load game from database.
    loadGame pool handle
