module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.All (AllRoutes)
import TeamTavern.Routes.CreateAlert (CreateAlert)
import TeamTavern.Routes.DeleteAlert (DeleteAlert)
import TeamTavern.Routes.ViewCompetitions (ViewCompetitions)
import TeamTavern.Server.Boarding.Routes (BoardRoutes)
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)
import TeamTavern.Server.Profile.Routes (ProfileRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> AllRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
    :<|> BoardRoutes
    :<|> "createAlert"      := CreateAlert
    :<|> "deleteAlert"      := DeleteAlert
    :<|> "viewCompetitions" := ViewCompetitions
