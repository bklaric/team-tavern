module TeamTavern.Profile.Routes where

import Data.Maybe (Maybe)
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (type (:?), NoQuery, Optional)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Player.Domain.Nickname (Nickname)

type IdentifiersSingle =
    { handle :: Handle
    , nickname :: Nickname
    }

type IdentifiersMany =
    { handle :: Maybe Handle
    , nickname :: Maybe Nickname
    }

type CreateProfile = Route
    Post
    (  Literal "profiles"
    :> Literal "single"
    :> Capture "handle" Handle
    :> End)
    NoQuery

type ViewAllProfiles = Route
    Get
    (  Literal "profiles"
    :> End)
    (  Optional "handle" Handle
    :? Optional "nickname" Nickname)

type ViewProfile = Route
    Get
    (  Literal "profiles"
    :> Literal "single"
    :> Capture "handle" Handle
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
    =    "createProfile"   := CreateProfile
    :<|> "viewAllProfiles" := ViewAllProfiles
    :<|> "viewProfile"     := ViewProfile
    :<|> "updateProfile"   := UpdateProfile
