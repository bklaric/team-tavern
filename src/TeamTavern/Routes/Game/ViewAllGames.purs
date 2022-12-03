module TeamTavern.Routes.Game.ViewAllGames where

import Jarilo.Types (Get)
import Jarilo.Types (Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (Ok)
import Jarilo.Types (FullRoute)

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
