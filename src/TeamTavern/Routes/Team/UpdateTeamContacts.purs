module TeamTavern.Routes.Team.UpdateTeamContacts where

import Data.Array.NonEmpty (NonEmptyArray)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Literal, NoContent, PutJson_)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)

type UpdateTeamContacts =
    PutJson_
    ( Literal "teams"
    / Capture "handle" String
    / Literal "contacts")
    RequestContent
    ==> NoContent ! BadRequestJson BadContent

type RequestContent = TeamContacts

type BadContent = NonEmptyArray TeamContactsError
