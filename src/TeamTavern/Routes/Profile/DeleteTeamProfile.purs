module TeamTavern.Routes.Profile.DeleteTeamProfile where

import Jarilo (type (!), type (/), type (==>), Capture, Delete_, Forbidden_, Internal_, Literal, NoContent, NotAuthorized_, NotFound_)

type DeleteTeamProfile =
    Delete_
    ( Literal "teams"
    / Capture "teamHandle" String
    / Literal "profiles"
    / Capture "gameHandle" String)
    ==> NoContent ! NotAuthorized_ ! Forbidden_ ! NotFound_ ! Internal_

type PathParams = { teamHandle :: String, gameHandle :: String }
