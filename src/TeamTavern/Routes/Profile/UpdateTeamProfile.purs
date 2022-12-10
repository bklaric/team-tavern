module TeamTavern.Routes.Profile.UpdateTeamProfile where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Internal_, Literal, NoContent, PutJson_, NotAuthorized_)
import TeamTavern.Routes.Profile.AddTeamProfile as AddTeamProfile
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdateTeamProfile =
    PutJson_
    ( Literal "teams"
    / Capture "teamHandle" Handle
    / Literal "profiles"
    / Capture "gameHandle" Nickname)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = AddTeamProfile.RequestContent

type BadContent = AddTeamProfile.BadContent
