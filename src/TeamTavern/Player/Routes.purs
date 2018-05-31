module TeamTavern.Player.Routes where

import Data.String.NonEmpty (NonEmptyString)
import Routing.Junction (type (:<|>), type (:=))
import Routing.Method (Get, Post)
import Routing.Path (type (:>), End)
import Routing.Query (NoQuery)
import Routing.Route (Route)
import Routing.Segment (Capture, Literal)

type ViewPlavers = Route Get (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get
    (Literal "players" :> Capture "nickname" NonEmptyString :> End)
    NoQuery

type RegisterPlayer = Route Post (Literal "players" :> End) NoQuery

type TeamTavernRoutes
    =    "viewPlayers"    := ViewPlavers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
