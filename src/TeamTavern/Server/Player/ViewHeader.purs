module TeamTavern.Server.Player.ViewHeader where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.ViewHeader.LoadHeader (loadHeader)
import TeamTavern.Server.Player.ViewHeader.LogError (logError)
import TeamTavern.Server.Player.ViewHeader.SendResponse (sendResponse)

viewHeader :: forall left. Pool -> Id -> Async left Response
viewHeader pool id =
    sendResponse $ examineLeftWithEffect logError do
    -- Load player header from database.
    loadHeader pool id