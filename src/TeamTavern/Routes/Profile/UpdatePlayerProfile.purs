module TeamTavern.Routes.Profile.UpdatePlayerProfile where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)
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
