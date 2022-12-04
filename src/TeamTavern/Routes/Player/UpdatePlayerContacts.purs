module TeamTavern.Routes.Player.UpdatePlayerContacts where

import Data.Array.NonEmpty (NonEmptyArray)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Literal, NoContent, NotAuthorized_, PutJson_, Internal_)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContacts, PlayerContactsError)

type UpdatePlayerContacts =
    PutJson_
    ( Literal "players"
    / Capture "nickname" String
    / Literal "contacts")
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = PlayerContacts

type BadContent = NonEmptyArray PlayerContactsError
