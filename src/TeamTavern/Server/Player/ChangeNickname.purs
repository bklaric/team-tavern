module TeamTavern.Server.Player.ChangeNickname where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Player.ChangeNickname.LogError (logError)
import TeamTavern.Server.Player.ChangeNickname.ReadNickname (readNickname)
import TeamTavern.Server.Player.ChangeNickname.SendResponse (sendResponse)
import TeamTavern.Server.Player.ChangeNickname.UpdateNickname (updateNickname)

changeNickname :: forall left.
    Pool -> String -> Cookies -> Body -> Async left Response
changeNickname pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies nickname

    -- Read update from body.
    update <- readNickname body

    -- Update player.
    updateNickname pool cookieInfo update
