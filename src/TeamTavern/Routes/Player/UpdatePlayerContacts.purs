module TeamTavern.Routes.Player.UpdatePlayerContacts where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Literal, NoContent, PutJson_)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)

type UpdatePlayerContacts =
    PutJson_
    ( Literal "players"
    / Capture "nickname" String
    / Literal "contacts")
    RequestContent
    ==> NoContent ! BadRequestJson BadContent

type RequestContent = PlayerContacts

type BadContent = Array PlayerContactsError
