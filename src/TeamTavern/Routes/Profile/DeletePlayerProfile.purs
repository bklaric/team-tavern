module TeamTavern.Routes.Profile.DeletePlayerProfile where

import Jarilo (type (!), type (/), type (==>), Capture, Delete_, Forbidden_, Internal_, Literal, NoContent, NotAuthorized_, NotFound_)

type DeletePlayerProfile =
    Delete_
    ( Literal "players"
    / Capture "nickname" String
    / Literal "profiles"
    / Capture "handle" String)
    ==> NoContent ! NotAuthorized_ ! Forbidden_ ! NotFound_ ! Internal_

type PathParams = { nickname :: String, handle :: String }
