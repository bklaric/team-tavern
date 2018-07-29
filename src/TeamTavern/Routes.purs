module TeamTavern.Routes where

import Jarilo.Junction (type (:<|>))
import TeamTavern.Player.Routes (PlayerRoutes)
import TeamTavern.Player.Session.Routes (SessionRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> SessionRoutes
