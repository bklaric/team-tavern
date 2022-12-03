module TeamTavern.Routes.Player.DeletePlayer where

import Jarilo (type (/), type (==>), Capture, Delete_, Literal, NoContent)

type DeletePlayer =
    Delete_ (Literal "players" / Capture "nickname" String)
    ==> NoContent
