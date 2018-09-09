module TeamTavern.Player.Update where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Map (Map)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Infrastructure.ReadAuth (readAuth)
import TeamTavern.Player.Infrastructure.ReadNickname (readNickname)
import TeamTavern.Player.Update.LogError (logError)
import TeamTavern.Player.Update.ReadUpdate (readUpdate)
import TeamTavern.Player.Update.Response (response)
import TeamTavern.Player.Update.UpdatePlayer (updatePlayer)

update :: forall left.
    Pool -> String -> Map String String -> Body -> Async left Response
update pool nickname' cookies body =
    response $ examineLeftWithEffect logError do
    -- Read nickname from route.
    nickname <- readNickname nickname'

    -- Read requestor auth info from cookies.
    authInfo <- readAuth cookies

    -- Read update from body.
    update' <- readUpdate body

    -- Update player.
    updatePlayer pool authInfo update'
