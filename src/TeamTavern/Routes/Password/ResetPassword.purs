module TeamTavern.Routes.Password.ResetPassword where

import Data.Variant (Variant)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, NoContent, PostJson_, NotFound_)

type ResetPassword =
    PostJson_ (Literal "reset-password") RequestContent
    ==> (NoContent ! BadRequestJson BadContent ! NotFound_ ! Internal_)

type RequestContent = {password :: String, nonce :: String}

type BadContent = Variant (password :: {})
