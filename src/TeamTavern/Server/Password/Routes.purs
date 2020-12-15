module TeamTavern.Server.Password.Routes where

import Jarilo.Junction (type (:=))
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

type ResetPassword = Route
    Post
    (  Literal "reset-password"
    :> End)
    NoQuery

type PasswordRoutes
    = "resetPassword"  := ResetPassword
