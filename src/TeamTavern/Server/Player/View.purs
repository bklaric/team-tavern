module TeamTavern.Server.Player.View where

import Prelude

import Async (Async, examineLeftWithEffect)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Player.View.LoadPlayer (loadPlayer)
import TeamTavern.Server.Player.View.LogError (logError)
import TeamTavern.Server.Player.View.SendResponse (sendResponse)

view :: forall left.
    Pool -> Cookies -> ViewPlayer.RouteParams -> Async left Response
view pool cookies routeParams =
    sendResponse $ examineLeftWithEffect logError do
    -- Check if player is signed in.
    cookieInfo <- checkSignedIn pool cookies

    -- Load player.
    loadPlayer pool cookieInfo routeParams
