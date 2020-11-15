module TeamTavern.Server.Wizard.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)
import TeamTavern.Routes.Onboarding (Onboard)
import TeamTavern.Routes.Preboarding (Preboard)

type CreateAccount = Route
    Post
    (  Literal "wizard"
    :> End)
    NoQuery

type WizardRoutes
    =    "createAccount" := CreateAccount
    :<|> "onboard"       := Onboard
    :<|> "preboard"      := Preboard
