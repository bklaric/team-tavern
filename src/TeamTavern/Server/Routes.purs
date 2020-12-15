module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>))
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Password.Routes (PasswordRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)
import TeamTavern.Server.Profile.Routes (ProfileRoutes)
import TeamTavern.Server.Session.Routes (SessionRoutes)
import TeamTavern.Server.Team.Routes (TeamRoutes)
import TeamTavern.Server.Boarding.Routes (WizardRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> TeamRoutes
    :<|> PasswordRoutes
    :<|> SessionRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
    :<|> WizardRoutes
