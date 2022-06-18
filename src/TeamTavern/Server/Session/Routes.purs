module TeamTavern.Server.Session.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Delete, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Literal)

type StartSession = FullRoute
    Post
    (  Literal "sessions"
    :> End)
    NoQuery

type EndSession = FullRoute
    Delete
    (  Literal "sessions"
    :> Literal "current"
    :> End)
    NoQuery

type SessionRoutes
    =    "startSession" := StartSession
    :<|> "endSession"   := EndSession
