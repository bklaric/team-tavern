module TeamTavern.Server.Player.View where

import Prelude

import Async (Async)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.View.LoadPlayer (loadPlayer)

view :: forall left.
    Pool -> Cookies -> ViewPlayer.RouteParams -> Async left _
view pool cookies routeParams =
    sendResponse "Error viewing player" do
    -- Check if player is signed in.
    cookieInfo <- checkSignedIn pool cookies

    -- Load player.
    ok_ <$> loadPlayer pool cookieInfo routeParams
