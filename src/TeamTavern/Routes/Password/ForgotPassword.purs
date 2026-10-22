module TeamTavern.Routes.Password.ForgotPassword where

import Jarilo (type (!), type (==>), Internal_, Literal, NoContent, NotFound_, PostJson_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Wait (Wait)

type ForgotPassword =
    PostJson_ (Literal "forgot-password") RequestContent
    ==> (NoContent ! NotFound_ ! TooManyRequestsJson Wait ! Internal_)

type RequestContent = {email :: String}
