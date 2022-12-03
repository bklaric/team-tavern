module TeamTavern.Routes.Profile.UpdatePlayerProfile where

import Jarilo.Types (Put)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, NoContent)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdatePlayerProfile = FullRoute
    (Put RequestContent)
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> Literal "profiles"
    :> Capture "handle" Handle)
    NoQuery
    (NoContent :! BadRequest BadContent)

type RequestContent = AddPlayerProfile.RequestContent

type BadContent = AddPlayerProfile.BadContent
