module TeamTavern.Routes.Profile.UpdatePlayerProfile where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Literal, NoContent, PutJson_)
import TeamTavern.Routes.Profile.AddPlayerProfile as AddPlayerProfile
import TeamTavern.Routes.Shared.Types (Handle, Nickname)

type UpdatePlayerProfile =
    PutJson_
    ( Literal "players"
    / Capture "nickname" Nickname
    / Literal "profiles"
    / Capture "handle" Handle)
    RequestContent
    ==> NoContent ! BadRequestJson BadContent

type RequestContent = AddPlayerProfile.RequestContent

type BadContent = AddPlayerProfile.BadContent
