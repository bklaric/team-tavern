module TeamTavern.Routes.Profile.UpdateTeamProfile where

import Jarilo.Types (Put)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, NoContent)
import Jarilo.Types (FullRoute)
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
