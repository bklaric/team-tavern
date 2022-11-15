module TeamTavern.Routes.Alert.DeleteAlert where

import Jarilo.Method (Delete)
import Jarilo.Path (type (:>), Capture, Literal)
import Jarilo.Query (Mandatory)
import Jarilo.Response (NoContent)
import Jarilo.Route (FullRoute)

type DeleteAlert = FullRoute
    Delete
    (  Literal "alerts"
    :> Capture "id" Int)
    (Mandatory "token" String)
    NoContent

type RouteContent = { id :: Int, token :: String }
