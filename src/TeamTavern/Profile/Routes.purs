module TeamTavern.Profile.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type CreateProfile = Route
    Post
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "profiles"
    :> End)
    NoQuery

type ViewProfile = Route
    Get
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "profiles"
    :> Capture "nickname" String
    :> End)
    NoQuery

type UpdateProfile = Route
    Put
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "profiles"
    :> Capture "nickname" String
    :> End)
    NoQuery

type ViewProfilesByGame = Route
    Get
    (  Literal "games"
    :> Capture "handle" String
    :> Literal "profiles"
    :> End)
    NoQuery

type ProfileRoutes
    =    "createProfile"      := CreateProfile
    :<|> "viewProfile"        := ViewProfile
    :<|> "updateProfile"      := UpdateProfile
    :<|> "viewProfilesByGame" := ViewProfilesByGame
