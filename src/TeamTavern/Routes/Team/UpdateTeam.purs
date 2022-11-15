module TeamTavern.Routes.Team.UpdateTeam where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, Ok)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Team.CreateTeam as CreateTeam

type UpdateTeam = FullRoute
    (Put RequestContent)
    (  Literal "teams"
    :> Capture "handle" String)
    NoQuery
    (Ok OkContent :! BadRequest BadContent)

type RequestContent = CreateTeam.RequestContent

type OkContent = { handle :: String }

type BadContentError = CreateTeam.BadContentError

type BadContent = CreateTeam.BadContent
