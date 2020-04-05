module TeamTavern.Server.Player.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type ViewPlayer = Route
    Get
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type ViewPlayerHeader = Route
    Get
    (  Literal "players"
    :> Capture "id" Id
    :> Literal "header"
    :> End)
    NoQuery

type RegisterPlayer = Route
    Post
    (  Literal "players"
    :> End)
    NoQuery

type ViewDetails = Route
    Get
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" Nickname
    :> Literal "details"
    :> End)
    NoQuery

type ChangeNickname = Route
    Put
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" String
    :> Literal "nickname"
    :> End)
    NoQuery

type ViewSettings = Route
    Get
    (  Literal "players"
    :> Literal "by-nickname"
    :> Capture "nickname" String
    :> Literal "settings"
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
    :<|> "viewPlayerHeader"  := ViewPlayerHeader
    :<|> "registerPlayer"    := RegisterPlayer
    :<|> "viewDetails"       := ViewDetails
    :<|> "changeNickname"    := ChangeNickname
    :<|> "viewSettings"      := ViewSettings
    :<|> "editSettings"      := EditSettings
    :<|> "updateDetails"     := UpdateDetails
