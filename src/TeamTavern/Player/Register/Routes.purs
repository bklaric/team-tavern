module TeamTavern.Player.Register.Routes where

import Routing.Method (Post)
import Routing.Path (type (:>), End)
import Routing.Route (Route)
import Routing.Query (NoQuery)
import Routing.Segment (Literal)

type RegisterPlayer = Route Post (Literal "players" :> End) NoQuery
