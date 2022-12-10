module TeamTavern.Routes.Player.DeletePlayer where

import Jarilo (type (!), type (/), type (==>), Capture, Delete_, Forbidden_, Internal_, Literal, NoContent, NotAuthorized_, NotFound_)

type DeletePlayer =
    Delete_ (Literal "players" / Capture "nickname" String)
    ==> NoContent ! NotAuthorized_ ! Forbidden_ ! NotFound_ ! Internal_
