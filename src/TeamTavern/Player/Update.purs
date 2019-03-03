module TeamTavern.Player.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Infrastructure.ReadCookieInfo (readCookieInfo)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Update.LogError (logError)
import TeamTavern.Player.Update.ReadUpdate (readUpdate)
import TeamTavern.Player.Update.SendResponse (sendResponse)
import TeamTavern.Player.Update.UpdatePlayer (updatePlayer)

update :: forall left.
    Pool -> Nickname -> Map String String -> Body -> Async left Response
update pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor info from cookies.
    cookieInfo <- readCookieInfo cookies

    -- Read update from body.
    update' <- readUpdate body

    -- Update player.
    updatePlayer pool cookieInfo update'
