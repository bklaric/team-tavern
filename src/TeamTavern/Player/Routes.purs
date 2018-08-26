module TeamTavern.Player.Routes where

import Data.String.NonEmpty (NonEmptyString)
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewPlayers = Route
    Get
    (  Literal "players"
    :> End)
    NoQuery

type ViewPlayer = Route
    Get
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" NonEmptyString
    :> End)
    NoQuery

type UpdatePlayer = Route
    Put
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" NonEmptyString
    :> End)
    NoQuery

type RegisterPlayer = Route
    Post
    (  Literal "players"
    :> End)
    NoQuery

type PlayerRoutes
    =    "viewPlayers"    := ViewPlayers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
    :<|> "updatePlayer"   := UpdatePlayer
