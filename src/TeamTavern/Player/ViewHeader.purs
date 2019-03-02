module TeamTavern.Player.ViewHeader where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Player.ViewHeader.LoadHeader (PlayerId(..), loadHeader)
import TeamTavern.Player.ViewHeader.LogError (logError)
import TeamTavern.Player.ViewHeader.SendResponse (sendResponse)

viewHeader :: forall left. Pool -> Int -> Async left Response
viewHeader pool id =
    sendResponse $ examineLeftWithEffect logError do
    -- Load player header from database.
    loadHeader pool (PlayerId id)
