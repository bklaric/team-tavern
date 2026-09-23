module TeamTavern.Routes.Password.ForgotPassword where

import Jarilo (type (!), type (==>), Internal_, Literal, NoContent, NotFound_, PostJson_)

type ForgotPassword =
    PostJson_ (Literal "forgot-password") RequestContent
    ==> (NoContent ! NotFound_ ! Internal_)

type RequestContent = {email :: String}
