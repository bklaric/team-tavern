module TeamTavern.Routes.Player.UpdatePlayerContacts where

import Jarilo.Types (Put)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, NoContent)
import Jarilo.Types (FullRoute)
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
