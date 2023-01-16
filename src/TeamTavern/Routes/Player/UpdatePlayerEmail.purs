module TeamTavern.Routes.Player.UpdatePlayerEmail where

import Data.Variant (Variant)
import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Forbidden_, Literal, NoContent, NotAuthorized_, PutJson_, Internal_)

type UpdatePlayerEmail =
    PutJson_
    ( Literal "players"
    / Capture "nickname" String
    / Literal "email")
    RequestContent
    ==> NoContent ! BadRequestJson BadContent ! NotAuthorized_ ! Forbidden_ ! Internal_

type RequestContent = {email :: String, password :: String}

type BadContent = Variant (email :: {}, emailTaken :: {}, wrongPassword :: {})
