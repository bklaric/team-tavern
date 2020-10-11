module TeamTavern.Server.Team.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory, NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewTeamsByOwner = Route
    Get
    (  Literal "teams"
    :> Literal "by-owner"
    :> Capture "nickname" String
    :> End)
    NoQuery

type ViewTeam = Route
    Get
    (  Literal "teams"
    :> Literal "by-handle"
    :> Capture "handle" String
    :> End)
    (Mandatory "timezone" String)

type CreateTeam = Route
    Post
    (  Literal "teams"
    :> End)
    NoQuery

type TeamRoutes
    =    "viewTeamsByOwner" := ViewTeamsByOwner
    :<|> "viewTeam"         := ViewTeam
    :<|> "createTeam"       := CreateTeam
