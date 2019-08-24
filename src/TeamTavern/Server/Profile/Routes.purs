module TeamTavern.Server.Profile.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery, Rest)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type Identifiers =
    { handle :: Handle
    , nickname :: Nickname
    }

type CreateProfile = Route
    Post
    (  Literal "profiles"
    :> Literal "single"
    :> Capture "handle" Handle
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type ViewProfilesByGame = Route
    Get
    (  Literal "profiles"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> End)
    (Rest "filters")

type ViewProfilesByPlayer = Route
    Get
    (  Literal "profiles"
    :> Literal "by-nickname"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type UpdateProfile = Route
    Put
    (  Literal "profiles"
    :> Literal "single"
    :> Capture "handle" Handle
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type ProfileRoutes
    =    "createProfile"        := CreateProfile
    :<|> "viewProfilesByGame"   := ViewProfilesByGame
    :<|> "viewProfilesByPlayer" := ViewProfilesByPlayer
    :<|> "updateProfile"        := UpdateProfile
