module TeamTavern.Player.Routes where

import Data.String.NonEmpty (NonEmptyString)
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Patch, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewPlayers = Route Get (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get
    (Literal "players" :> Capture "nickname" NonEmptyString :> End)
    NoQuery

type RegisterPlayer = Route
    Post
    (  Literal "players"
    :> End)
    NoQuery

type PrepareSession = Route
    Post
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" NonEmptyString
    :> Literal "sessions"
    :> End)
    NoQuery

type StartSession = Route
    Patch
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" NonEmptyString
    :> Literal "sessions"
    :> End)
    NoQuery

type TeamTavernRoutes
    =    "viewPlayers"    := ViewPlayers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
    :<|> "prepareSession" := PrepareSession
    :<|> "startSession"   := StartSession
