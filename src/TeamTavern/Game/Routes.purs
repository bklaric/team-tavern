module TeamTavern.Game.Routes where

import Jarilo.Junction (type (:=))
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Literal)

type CreateGame = Route
    Post
    (  Literal "games"
    :> End)
    NoQuery

type GameRoutes
    = "createGame" := CreateGame
