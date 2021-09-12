module TeamTavern.Routes.UpdatePlayerContacts where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)

type UpdatePlayerContacts = Route
    Put
    (  Literal "players"
    :> Capture "nickname" String
    :> Literal "contacts"
    :> End)
    NoQuery

type RequestContent = PlayerContacts

type BadContent = Array PlayerContactsError
