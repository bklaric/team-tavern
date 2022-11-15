module TeamTavern.Routes.Profile.UpdateTeamProfile where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Profile.AddTeamProfile as AddTeamProfile
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdateTeamProfile = FullRoute
    (Put RequestContent)
    (  Literal "teams"
    :> Capture "teamHandle" Handle
    :> Literal "profiles"
    :> Capture "gameHandle" Nickname)
    NoQuery
    (NoContent :! BadRequest BadContent)

type RequestContent = AddTeamProfile.RequestContent

type BadContent = AddTeamProfile.BadContent
