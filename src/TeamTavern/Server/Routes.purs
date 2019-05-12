module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>))
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)
import TeamTavern.Server.Session.Routes (SessionRoutes)
import TeamTavern.Server.Profile.Routes (ProfileRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> SessionRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
