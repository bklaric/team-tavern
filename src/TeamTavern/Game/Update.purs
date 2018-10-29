module TeamTavern.Game.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Infrastructure.ReadDetails (readDetails)
import TeamTavern.Game.Infrastructure.ReadHandle (readHandle)
import TeamTavern.Game.Update.LogError (logError)
import TeamTavern.Game.Update.Response (response)
import TeamTavern.Game.Update.UpdateGame (updateGame)
import TeamTavern.Infrastructure.ReadAuth (readAuth)

handleUpdate :: forall left.
    Pool -> String -> Map String String -> Body -> Async left Response
handleUpdate pool targetHandle' cookies body =
    response $ examineLeftWithEffect logError do
    -- Read target handle from route.
    targetHandle <- readHandle targetHandle'

    -- Read requestor authentication info from cookie.
    authInfo <- readAuth cookies

    -- Read update from body.
    details <- readDetails body

    -- Update game.
    updateGame pool authInfo targetHandle details
