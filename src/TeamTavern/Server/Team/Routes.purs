module TeamTavern.Server.Team.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
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
    NoQuery

type TeamRoutes
    =    "viewTeamsByOwner" := ViewTeamsByOwner
    -- :<|> "viewTeam"         := ViewTeam
