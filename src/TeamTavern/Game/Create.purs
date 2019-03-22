module TeamTavern.Game.Create where

import Prelude

import Async (Async)
import Async (examineLeftWithEffect) as Async
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Game.Create.AddGame (addGame)
import TeamTavern.Game.Create.LogError (logError)
import TeamTavern.Game.Create.SendResponse (sendResponse)
import TeamTavern.Game.Infrastructure.ReadModel (readModel)
import TeamTavern.Infrastructure.ReadCookieInfo (readCookieInfo)

create :: forall left. Pool -> Map String String -> Body -> Async left Response
create pool cookies body =
    sendResponse $ Async.examineLeftWithEffect logError do
    -- Read player creating the game.
    cookieInfo <- readCookieInfo cookies

    -- Read game title and description.
    details <- readModel body

    -- Add game to database.
    addGame pool cookieInfo details
