module TeamTavern.Routes.Password.ForgotPassword where

import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Forbidden_, Internal_, Literal, NoContent, PostJson_)

type ForgotPassword =
    PostJson_ (Literal "forgot-password") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Forbidden_ ! Internal_)

type RequestContent = {email :: String}

type BadContent = Variant (unknownEmail :: {})
