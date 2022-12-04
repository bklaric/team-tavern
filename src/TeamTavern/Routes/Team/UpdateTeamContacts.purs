module TeamTavern.Routes.Team.UpdateTeamContacts where

import Data.Array.NonEmpty (NonEmptyArray)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Literal, NoContent, NotAuthorized_, PutJson_, Internal_)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)

type UpdateTeamContacts =
    PutJson_
    ( Literal "teams"
    / Capture "handle" String
    / Literal "contacts")
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = TeamContacts

type BadContent = NonEmptyArray TeamContactsError
