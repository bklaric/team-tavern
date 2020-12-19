module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>))
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)
import TeamTavern.Server.Profile.Routes (ProfileRoutes)
import TeamTavern.Server.Session.Routes (SessionRoutes)
import TeamTavern.Server.Team.Routes (TeamRoutes)
import TeamTavern.Server.Boarding.Routes (WizardRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> TeamRoutes
    :<|> SessionRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
    :<|> WizardRoutes
