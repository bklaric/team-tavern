module TeamTavern.Player.Routes where

import Data.String.NonEmpty (NonEmptyString)
import Routing.Junction (type (:<|>), type (:=))
import Routing.Method (Get)
import Routing.Path (type (:>), End)
import Routing.Query (NoQuery)
import Routing.Route (Route)
import Routing.Segment (Capture, Literal)
import TeamTavern.Player.Register.Routes (RegisterPlayer)

type ViewPlavers = Route Get (Literal "players" :> End) NoQuery

type ViewPlayer = Route Get
    (Literal "players" :> Capture "nickname" NonEmptyString :> End)
    NoQuery

type TeamTavernRoutes
    =    "viewPlayers"    := ViewPlavers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
