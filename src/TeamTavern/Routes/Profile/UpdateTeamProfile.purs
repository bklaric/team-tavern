module TeamTavern.Routes.Profile.UpdateTeamProfile where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdateTeamProfile = FullRoute
    Put
    (  Literal "teams"
    :> Capture "teamHandle" Handle
    :> Literal "profiles"
    :> Capture "gameHandle" Nickname
    :> End)
    NoQuery
