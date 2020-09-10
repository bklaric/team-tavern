module TeamTavern.Server.Wizard.Routes where

import Jarilo.Junction (type (:=))
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

type WizardRoutes
    =    "createAccount" := CreateAccount
