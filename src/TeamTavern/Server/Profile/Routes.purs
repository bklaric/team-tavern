module TeamTavern.Server.Profile.Routes where

import Data.Maybe (Maybe)
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (type (:?), Mandatory, Many, NoQuery, Optional, Rest)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

type ProfileIlk = Int

type ProfilePage = Int

type Age = Int

type HasMicrophone = Boolean

type Language = String

type Filters =
    { age :: { ageFrom :: Maybe Age, ageTo :: Maybe Age }
    , microphone :: HasMicrophone
    , languages :: Array Language
    , fields :: QueryPairs Key Value
    }

type Handle = String

type Nickname = String

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
    (  Mandatory "ilk" ProfileIlk
    :? Mandatory "page" ProfilePage
    :? Optional "ageFrom" Age
    :? Optional "ageTo" Age
    :? Optional "microphone" HasMicrophone
    :? Many "languages" Language
    :? Rest "fields")

type ViewProfilesByPlayer = Route
    Get
    (  Literal "profiles"
    :> Literal "by-nickname"
    :> Capture "nickname" Nickname
    :> End)
    (Mandatory "ilk" ProfileIlk)

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
