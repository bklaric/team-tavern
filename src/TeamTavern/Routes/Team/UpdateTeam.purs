module TeamTavern.Routes.Team.UpdateTeam where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Internal_, Literal, NoContent, NotAuthorized_, PutJson_)
import TeamTavern.Routes.Team.CreateTeam as CreateTeam

type UpdateTeam =
    PutJson_
    ( Literal "teams"
    / Capture "handle" String)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = CreateTeam.RequestContent

type BadContent = CreateTeam.BadContent
