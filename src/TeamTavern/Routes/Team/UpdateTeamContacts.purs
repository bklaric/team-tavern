module TeamTavern.Routes.Team.UpdateTeamContacts where

import Jarilo.Types (Put)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, NoContent)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)

type UpdateTeamContacts = FullRoute
    (Put RequestContent)
    (  Literal "teams"
    :> Capture "handle" String
    :> Literal "contacts")
    NoQuery
    (NoContent :! BadRequest BadContent)

type RequestContent = TeamContacts

type BadContent = Array TeamContactsError
