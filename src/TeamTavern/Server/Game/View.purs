module TeamTavern.Server.Game.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.View.LoadGame (loadGame)
import TeamTavern.Server.Game.View.LogError (logError)
import TeamTavern.Server.Game.View.SendResponse (sendResponse)

handleView :: forall left.
    Pool -> Handle -> Map String String -> Async left Response
handleView pool handle cookies =
    sendResponse $ examineLeftWithEffect logError do
    -- Load game from database.
    loadGame pool handle
