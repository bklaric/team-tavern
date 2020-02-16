module TeamTavern.Server.Profile.Routes where

import Prelude

import Data.Maybe (Maybe, maybe)
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

type Time = String

type Language = String

type Filters =
    { age :: { from :: Maybe Age, to :: Maybe Age }
    , microphone :: HasMicrophone
    , languages :: Array Language
    , weekdayOnline :: { from :: Maybe String, to :: Maybe String }
    , weekendOnline :: { from :: Maybe String, to :: Maybe String }
    , fields :: QueryPairs Key Value
    }

bundleFilters :: forall other.
    { ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , fields :: QueryPairs Key Value
    , languages :: Array String
    , microphone :: Maybe Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    | other }
    -> Filters
bundleFilters filters =
    { age: { from: filters.ageFrom, to: filters.ageTo }
    , microphone: maybe false identity filters.microphone
    , languages: filters.languages
    , weekdayOnline: { from: filters.weekdayFrom, to: filters.weekdayTo }
    , weekendOnline: { from: filters.weekendFrom, to: filters.weekendTo }
    , fields: filters.fields
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
    :? Optional "weekdayFrom" Time
    :? Optional "weekdayTo" Time
    :? Optional "weekendFrom" Time
    :? Optional "weekendTo" Time
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
