module TeamTavern.Routes.Session.EndSession where

import Jarilo (type (/), type (==>), Delete_, Literal, NoContent)

type EndSession =
    Delete_ (Literal "sessions" / Literal "current")
    ==> NoContent
