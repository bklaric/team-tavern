module TeamTavern.Routes.Player.ResendConfirmation where

import Jarilo (type (!), type (/), type (==>), Internal_, Literal, NoBody, NoContent, NotAuthorized_, Post_)

type ResendConfirmation =
    Post_ (Literal "confirm-email" / Literal "resend") NoBody
    ==> (NoContent ! NotAuthorized_ ! Internal_)
