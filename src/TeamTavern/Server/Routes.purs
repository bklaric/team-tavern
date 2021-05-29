module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.CreateAlert (CreateAlert)
import TeamTavern.Routes.DeleteAlert (DeleteAlert)
import TeamTavern.Routes.ViewCompetitions (ViewCompetitions)
import TeamTavern.Server.Boarding.Routes (BoardRoutes)
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)
import TeamTavern.Server.Profile.Routes (ProfileRoutes)
import TeamTavern.Server.Session.Routes (SessionRoutes)
import TeamTavern.Server.Team.Routes (TeamRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> TeamRoutes
    :<|> SessionRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
    :<|> BoardRoutes
    :<|> "createAlert"      := CreateAlert
    :<|> "deleteAlert"      := DeleteAlert
    :<|> "viewCompetitions" := ViewCompetitions
