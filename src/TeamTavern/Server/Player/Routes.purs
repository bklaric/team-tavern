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

type EditSettings = Route
    Put
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" String
    :> Literal "settings"
    :> End)
    NoQuery

type UpdateDetails = Route
    Put
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" Nickname
    :> Literal "details"
    :> End)
    NoQuery

type PlayerRoutes
    =    "viewPlayer"        := ViewPlayer
    :<|> "registerPlayer"    := RegisterPlayer
    :<|> "editSettings"      := EditSettings
    :<|> "updateDetails"     := UpdateDetails
