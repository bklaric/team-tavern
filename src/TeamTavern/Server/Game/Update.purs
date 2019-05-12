module TeamTavern.Server.Game.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Game.Infrastructure.ReadModel (readModel)
import TeamTavern.Server.Game.Update.LogError (logError)
import TeamTavern.Server.Game.Update.SendResponse (sendResponse)
import TeamTavern.Server.Game.Update.UpdateGame (updateGame)
import TeamTavern.Server.Infrastructure.ReadCookieInfo (readCookieInfo)

handleUpdate :: forall left.
    Pool -> Handle -> Map String String -> Body -> Async left Response
handleUpdate pool targetHandle cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor authentication info from cookie.
    cookieInfo <- readCookieInfo cookies

    -- Read update from body.
    model <- readModel body

    -- Update game.
    updateGame pool cookieInfo targetHandle model
