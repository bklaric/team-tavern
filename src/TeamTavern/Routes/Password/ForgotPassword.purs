module TeamTavern.Routes.Password.ForgotPassword where

import Jarilo (type (!), type (==>), Forbidden_, Internal_, Literal, NoContent, NotFound_, PostJson_)

type ForgotPassword =
    PostJson_ (Literal "forgot-password") RequestContent
    ==> (NoContent ! Forbidden_ ! NotFound_ ! Internal_)

type RequestContent = {email :: String}
