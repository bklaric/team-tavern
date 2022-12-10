module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Forbidden_, Internal_, Literal, NoContent, PostJson_)

type StartSession =
    PostJson_ (Literal "sessions") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Forbidden_ ! Internal_)

type RequestContent =
    { nickname :: String
    , password :: String
    }

type BadContent = Variant (noSessionStarted :: {})
