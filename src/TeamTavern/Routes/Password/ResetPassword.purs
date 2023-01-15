module TeamTavern.Routes.Password.ResetPassword where

import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Forbidden_, Internal_, Literal, NoContent, PostJson_)

type ResetPassword =
    PostJson_ (Literal "reset-password") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! Forbidden_ ! Internal_)

type RequestContent = {password :: String, nonce :: String}

type BadContent = Variant (password :: {}, nonce :: {})
