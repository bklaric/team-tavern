module TeamTavern.Routes.Player.DeletePlayer where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (NoContent)
import Jarilo.Route (FullRoute)

type DeletePlayer = FullRoute
    Delete
    (  Literal "players"
    :> Capture "nickname" String)
    NoQuery
    NoContent
