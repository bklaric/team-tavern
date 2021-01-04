module TeamTavern.Server.Game.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.ViewAllGames (ViewAllGames)
import TeamTavern.Routes.ViewGame (ViewGame)

type GameRoutes
    =    "viewAllGames" := ViewAllGames
    :<|> "viewGame"     := ViewGame
