module TeamTavern.Routes.Profile.UpdatePlayerProfile where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Internal_, Literal, NoContent, NotAuthorized_, PutJson_, Forbidden_)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdatePlayerProfile =
    PutJson_
    ( Literal "players"
    / Capture "nickname" Nickname
    / Literal "profiles"
    / Capture "handle" Handle)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = AddPlayerProfile.RequestContent

type BadContent = AddPlayerProfile.BadContent
