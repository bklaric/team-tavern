module TeamTavern.Server.Wizard.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

type CreateAccount = Route
    Post
    (  Literal "wizard"
    :> End)
    NoQuery

type Onboard = Route
    Post
    (  Literal "wizard"
    :> Literal "onboard"
    :> End)
    NoQuery

type WizardRoutes
    =    "createAccount" := CreateAccount
    :<|> "onboard"       := Onboard
