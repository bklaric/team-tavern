module TeamTavern.Routes.DeletePlayer where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type DeletePlayer = Route
    Delete
    (  Literal "players"
    :> Capture "nickname" String
    :> End)
    NoQuery
