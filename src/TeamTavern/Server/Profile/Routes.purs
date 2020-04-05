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

type Timezone = String

type Age = Int

type Language = String

type Country = String

type Time = String

type HasMicrophone = Boolean

type Filters =
    { age :: { from :: Maybe Age, to :: Maybe Age }
    , languages :: Array Language
    , countries :: Array Country
    , weekdayOnline :: { from :: Maybe Time, to :: Maybe Time }
    , weekendOnline :: { from :: Maybe Time, to :: Maybe Time }
    , microphone :: HasMicrophone
    , fields :: QueryPairs Key Value
    }

bundleFilters :: forall other.
    { ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , fields :: QueryPairs Key Value
    , languages :: Array String
    , countries :: Array String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , microphone :: Maybe Boolean
    | other }
    -> Filters
bundleFilters filters =
    { age: { from: filters.ageFrom, to: filters.ageTo }
    , languages: filters.languages
    , countries: filters.countries
    , weekdayOnline: { from: filters.weekdayFrom, to: filters.weekdayTo }
    , weekendOnline: { from: filters.weekendFrom, to: filters.weekendTo }
    , microphone: maybe false identity filters.microphone
    , fields: filters.fields
    }

type Handle = String

type Nickname = String

type Identifiers =
    { handle :: Handle
    , nickname :: Nickname
    }

type AddGamePlayer = Route
    Post
    (  Literal "profiles"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> Literal "players"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type UpdateGamePlayer = Route
    Put
    (  Literal "profiles"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> Literal "players"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type ViewGamePlayers = Route
    Get
    (  Literal "profiles"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> Literal "players"
    :> End)
    (  Mandatory "page" ProfilePage
    :? Mandatory "timezone" Timezone
    :? Optional "ageFrom" Age
    :? Optional "ageTo" Age
    :? Many "languages" Language
    :? Many "countries" Country
    :? Optional "weekdayFrom" Time
    :? Optional "weekdayTo" Time
    :? Optional "weekendFrom" Time
    :? Optional "weekendTo" Time
    :? Optional "microphone" HasMicrophone
    :? Rest "fields")

type ViewProfilesByPlayer = Route
    Get
    (  Literal "profiles"
    :> Literal "by-nickname"
    :> Capture "nickname" Nickname
    :> End)
    (Mandatory "ilk" ProfileIlk)

type ProfileRoutes
    =    "createProfile"        := AddGamePlayer
    :<|> "updateProfile"        := UpdateGamePlayer
    :<|> "viewProfilesByGame"   := ViewGamePlayers
    :<|> "viewProfilesByPlayer" := ViewProfilesByPlayer
