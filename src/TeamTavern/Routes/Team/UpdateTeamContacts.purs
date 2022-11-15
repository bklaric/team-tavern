module TeamTavern.Routes.Team.UpdateTeamContacts where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)
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
