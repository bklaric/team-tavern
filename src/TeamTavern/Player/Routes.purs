module TeamTavern.Player.Routes where

import Data.String.NonEmpty (NonEmptyString)
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type ViewPlavers = Route Get (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get
    (Literal "players" :> Capture "nickname" NonEmptyString :> End)
    NoQuery

type RegisterPlayer = Route Post (Literal "players" :> End) NoQuery

type SignInPlayer = Route Post
    (Literal "players"
    :> Capture "nickname" NonEmptyString
    :> Literal "sessions"
    :> End)
    NoQuery

type TeamTavernRoutes
    =    "viewPlayers"    := ViewPlavers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
    :<|> "signInPlayer"   := SignInPlayer
