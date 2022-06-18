module TeamTavern.Routes.Team.UpdateTeam where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Team.CreateTeam as CreateTeam

type UpdateTeam = FullRoute
    Put
    (  Literal "teams"
    :> Capture "handle" String
    :> End)
    NoQuery

type RequestContent = CreateTeam.RequestContent

type OkContent = { handle :: String }

type BadContentError = CreateTeam.BadContentError

type BadContent = CreateTeam.BadContent
