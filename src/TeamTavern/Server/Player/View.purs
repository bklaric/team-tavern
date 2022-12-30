module TeamTavern.Server.Player.View where

import Prelude

import Async (Async)
import Data.Map (Map)
import Jarilo (ok_)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.SendResponse (lmapElaborateReferrer, sendResponse)
import TeamTavern.Server.Player.View.LoadPlayer (loadPlayer)

view :: ∀ left.
    Pool -> Cookies -> ViewPlayer.RouteParams -> Map String String -> Async left _
view pool cookies routeParams headers =
    sendResponse "Error viewing player" $ lmapElaborateReferrer headers do
    -- Check if player is signed in.
    cookieInfo <- checkSignedIn pool cookies

    -- Load player.
    ok_ <$> loadPlayer pool cookieInfo routeParams
