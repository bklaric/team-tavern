module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.All (AllRoutes)
import TeamTavern.Routes.CreateAlert (CreateAlert)
import TeamTavern.Routes.DeleteAlert (DeleteAlert)
import TeamTavern.Server.Boarding.Routes (BoardRoutes)
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> AllRoutes
    :<|> GameRoutes
    :<|> BoardRoutes
    :<|> "createAlert"      := CreateAlert
    :<|> "deleteAlert"      := DeleteAlert
