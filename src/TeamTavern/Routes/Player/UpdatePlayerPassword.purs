module TeamTavern.Routes.Player.UpdatePlayerPassword where

import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Literal, NoContent, NotAuthorized_, PutJson_, Internal_)

type UpdatePlayerPassword =
    PutJson_
    ( Literal "players"
    / Capture "nickname" String
    / Literal "password")
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = {passwordOld :: String, passwordNew :: String}

type BadContent = Variant (wrongPassword :: {}, password :: {})
