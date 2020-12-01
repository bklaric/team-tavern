module TeamTavern.Server.Game.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.ViewAllGames (ViewAllGames)
import TeamTavern.Server.Game.Domain.Handle (Handle)

type ViewGame = Route
    Get
    (  Literal "games"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> End)
    NoQuery

type GameRoutes
    =    "viewAllGames" := ViewAllGames
    :<|> "viewGame"     := ViewGame
