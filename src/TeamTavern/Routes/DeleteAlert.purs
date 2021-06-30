module TeamTavern.Routes.DeleteAlert where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (Mandatory)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type DeleteAlert = Route
    Delete
    (  Literal "alerts"
    :> Capture "id" Int
    :> End)
    (Mandatory "token" String)

type RouteContent = { id :: Int, token :: String }
