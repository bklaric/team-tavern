module TeamTavern.Routes.Alert.DeleteAlert where

import Jarilo (type (!), type (/), type (==>), Capture, Delete, Internal_, Literal, Mandatory, NoContent, NotFound_)

type DeleteAlert =
    Delete
    (Literal "alerts" / Capture "id" Int)
    (Mandatory "token" String)
    ==> NoContent ! NotFound_ ! Internal_

type RouteContent = { id :: Int, token :: String }
