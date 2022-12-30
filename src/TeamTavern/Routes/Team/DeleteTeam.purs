module TeamTavern.Routes.Team.DeleteTeam where

import Jarilo (type (!), type (/), type (==>), Capture, Delete_, Forbidden_, Internal_, Literal, NoContent, NotAuthorized_, NotFound_)

type DeleteTeam =
    Delete_ (Literal "teams" / Capture "handle" String)
    ==> NoContent ! NotAuthorized_ ! Forbidden_ ! NotFound_ ! Internal_
