module TeamTavern.Routes.Player.UpdatePlayerContacts where

import Jarilo.Method (Put)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, NoContent)
import Jarilo.Route (FullRoute)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)

type UpdatePlayerContacts = FullRoute
    (Put RequestContent)
    (  Literal "players"
    :> Capture "nickname" String
    :> Literal "contacts")
    NoQuery
    (NoContent :! BadRequest BadContent)

type RequestContent = PlayerContacts

type BadContent = Array PlayerContactsError
