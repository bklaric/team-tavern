module TeamTavern.Server.Player.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.ViewPlayer (ViewPlayer)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type RegisterPlayer = Route
    Post
    (  Literal "players"
    :> End)
    NoQuery

type UpdatePlayer = Route
    Put
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type PlayerRoutes
    =    "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
    :<|> "updatePlayer"   := UpdatePlayer
