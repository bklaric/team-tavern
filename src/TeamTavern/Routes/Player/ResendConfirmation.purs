module TeamTavern.Routes.Player.ResendConfirmation where

import Jarilo (type (!), type (/), type (==>), Internal_, Literal, NoBody, NoContent, NotAuthorized_, Post_, TooManyRequestsJson)
import TeamTavern.Routes.Shared.Wait (Wait)

type ResendConfirmation =
    Post_ (Literal "confirm-email" / Literal "resend") NoBody
    ==> (NoContent ! NotAuthorized_ ! TooManyRequestsJson Wait ! Internal_)
