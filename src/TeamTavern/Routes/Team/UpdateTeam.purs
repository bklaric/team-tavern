module TeamTavern.Routes.Team.UpdateTeam where

import Jarilo.Types (Put)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, Ok)
import Jarilo.Types (FullRoute)
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
