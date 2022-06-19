module TeamTavern.Routes.Profile.UpdatePlayerProfile where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdatePlayerProfile = FullRoute
    Put
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> Literal "profiles"
    :> Capture "handle" Handle
    :> End)
    NoQuery
