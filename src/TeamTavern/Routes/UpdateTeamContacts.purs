module TeamTavern.Routes.UpdateTeamContacts where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)

type UpdateTeamContacts = Route
    Put
    (  Literal "teams"
    :> Capture "handle" String
    :> Literal "contacts"
    :> End)
    NoQuery

type RequestContent = TeamContacts

type BadContent = Array TeamContactsError
