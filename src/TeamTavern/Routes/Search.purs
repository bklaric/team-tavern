module TeamTavern.Routes.Search where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (==>), Get, Internal_, Literal, Mandatory, OkJson)

type Search =
    Get (Literal "search") (Mandatory "query" String)
    ==> OkJson OkContent ! Internal_

type OkContent =
    { players :: Array { nickname :: String }
    , teams :: Array { handle :: String, name :: Maybe String }
    }
