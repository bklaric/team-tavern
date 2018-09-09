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
import TeamTavern.Game.Create.Response (response)
import TeamTavern.Game.Infrastructure.ReadDetails (readDetails)
import TeamTavern.Infrastructure.ReadAuth (readAuth)

create :: forall left. Pool -> Map String String -> Body -> Async left Response
create pool cookies body =
    response $ Async.examineLeftWithEffect logError do
    -- Read player creating the game.
    authInfo <- readAuth cookies

    -- Read game name and description.
    details <- readDetails body

    -- Add game to database.
    addGame pool authInfo details
