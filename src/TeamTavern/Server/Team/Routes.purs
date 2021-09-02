module TeamTavern.Server.Team.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory, NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.UpdateTeamContacts (UpdateTeamContacts)

type ViewTeam = Route
    Get
    (  Literal "teams"
    :> Capture "handle" String
    :> End)
    (Mandatory "timezone" String)

type CreateTeam = Route
    Post
    (  Literal "teams"
    :> End)
    NoQuery

type UpdateTeam = Route
    Put
    (  Literal "teams"
    :> Capture "handle" String
    :> End)
    NoQuery

type TeamRoutes
    =    "viewTeam"   := ViewTeam
    :<|> "createTeam" := CreateTeam
    :<|> "updateTeam" := UpdateTeam
    :<|> "updateTeamContacts" := UpdateTeamContacts
