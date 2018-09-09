module TeamTavern.Session.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Patch, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

type PrepareSession = Route
    Post
    (  Literal "sessions"
    :> End)
    NoQuery

type StartSession = Route
    Patch
    (  Literal "sessions"
    :> End)
    NoQuery

type SessionRoutes
    =    "prepareSession" := PrepareSession
    :<|> "startSession"   := StartSession
