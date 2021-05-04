module TeamTavern.Server.Boarding.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.Onboard (Onboard)
import TeamTavern.Routes.Preboard (Preboard)

type BoardRoutes
    =    "onboard"       := Onboard
    :<|> "preboard"      := Preboard
