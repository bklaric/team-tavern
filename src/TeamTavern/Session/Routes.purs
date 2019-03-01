module TeamTavern.Session.Routes where

import Jarilo.Junction (type (:=))
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

type StartSession = Route
    Post
    (  Literal "sessions"
    :> End)
    NoQuery

type SessionRoutes = "startSession"   := StartSession
