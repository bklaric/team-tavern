module TeamTavern.Player.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewPlayer = Route
    Get
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" String
    :> End)
    NoQuery

type ViewPlayerHeader = Route
    Get
    (  Literal "players"
    :> Capture "id" Int
    :> Literal "header"
    :> End)
    NoQuery

type RegisterPlayer = Route
    Post
    (  Literal "players"
    :> End)
    NoQuery

type UpdatePlayer = Route
    Put
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" String
    :> End)
    NoQuery

type PlayerRoutes
    =    "viewPlayer"       := ViewPlayer
    :<|> "viewPlayerHeader" := ViewPlayerHeader
    :<|> "registerPlayer"   := RegisterPlayer
    :<|> "updatePlayer"     := UpdatePlayer
