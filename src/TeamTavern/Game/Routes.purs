module TeamTavern.Game.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Game.Domain.Handle (Handle)

type CreateGame = Route
    Post
    (  Literal "games"
    :> End)
    NoQuery

type ViewAllGames = Route
    Get
    (  Literal "games"
    :> End)
    NoQuery

type ViewGame = Route
    Get
    (  Literal "games"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> End)
    NoQuery

type UpdateGame = Route
    Put
    (  Literal "games"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> End)
    NoQuery

type GameRoutes
    =    "createGame"   := CreateGame
    :<|> "viewAllGames" := ViewAllGames
    :<|> "viewGame"     := ViewGame
    :<|> "updateGame"   := UpdateGame
