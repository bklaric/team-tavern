module TeamTavern.Routes.Game.ViewAllGames where

import Jarilo.Method (Get)
import Jarilo.Path (Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (Ok)
import Jarilo.Route (FullRoute)

type ViewAllGames = FullRoute
    Get
    (  Literal "games")
    NoQuery
    (Ok OkContent)

type OkGameContent =
    { title :: String
    , shortTitle :: String
    , handle :: String
    , description :: Array String
    }

type OkContent = Array OkGameContent
