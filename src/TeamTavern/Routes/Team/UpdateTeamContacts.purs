module TeamTavern.Routes.Team.UpdateTeamContacts where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)

type UpdateTeamContacts = FullRoute
    Put
    (  Literal "teams"
    :> Capture "handle" String
    :> Literal "contacts"
    :> End)
    NoQuery

type RequestContent = TeamContacts

type BadContent = Array TeamContactsError
