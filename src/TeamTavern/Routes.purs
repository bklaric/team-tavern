module TeamTavern.Routes where

import Jarilo.Junction (type (:<|>))
import TeamTavern.Game.Routes (GameRoutes)
import TeamTavern.Player.Routes (PlayerRoutes)
import TeamTavern.Session.Routes (SessionRoutes)
import TeamTavern.Profile.Routes (ProfileRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> SessionRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
