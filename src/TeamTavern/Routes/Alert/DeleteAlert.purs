module TeamTavern.Routes.Alert.DeleteAlert where

import Jarilo (type (/), type (==>), Capture, Delete, Literal, Mandatory, NoContent)

type DeleteAlert =
    Delete
    (Literal "alerts" / Capture "id" Int)
    (Mandatory "token" String)
    ==> NoContent

type RouteContent = { id :: Int, token :: String }
