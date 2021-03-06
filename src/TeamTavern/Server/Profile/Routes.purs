module TeamTavern.Server.Profile.Routes where

import Prelude

import Data.Maybe (Maybe, maybe)
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (type (:?), Mandatory, Many, NoQuery, Optional, Rest)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.Platform (Platform)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

type ProfileIlk = Int

type ProfilePage = Int

type Timezone = String

type Age = Int

type Language = String

type Location = String

type Time = String

type HasMicrophone = Boolean

type NewOrReturning = Boolean

type Filters =
    { age :: { from :: Maybe Age, to :: Maybe Age }
    , languages :: Array Language
    , locations :: Array Location
    , weekdayOnline :: { from :: Maybe Time, to :: Maybe Time }
    , weekendOnline :: { from :: Maybe Time, to :: Maybe Time }
    , microphone :: HasMicrophone
    , platforms :: Array Platform
    , fields :: QueryPairs Key Value
    , newOrReturning :: NewOrReturning
    }

bundleFilters :: forall other.
    { ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , languages :: Array String
    , locations :: Array String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , microphone :: Maybe Boolean
    , platforms :: Array Platform
    , fields :: QueryPairs Key Value
    , newOrReturning :: Maybe Boolean
    | other }
    -> Filters
bundleFilters filters =
    { age: { from: filters.ageFrom, to: filters.ageTo }
    , languages: filters.languages
    , locations: filters.locations
    , weekdayOnline: { from: filters.weekdayFrom, to: filters.weekdayTo }
    , weekendOnline: { from: filters.weekendFrom, to: filters.weekendTo }
    , microphone: maybe false identity filters.microphone
    , platforms: filters.platforms
    , fields: filters.fields
    , newOrReturning: maybe false identity filters.newOrReturning
    }

type Handle = String

type Nickname = String

type Identifiers =
    { handle :: Handle
    , nickname :: Nickname
    }

type AddPlayerProfile = Route
    Post
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> Literal "profiles"
    :> Capture "handle" Handle
    :> End)
    NoQuery

type AddTeamProfile = Route
    Post
    (  Literal "teams"
    :> Capture "teamHandle" Handle
    :> Literal "profiles"
    :> Capture "gameHandle" Handle
    :> End)
    NoQuery

type UpdatePlayerProfile = Route
    Put
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> Literal "profiles"
    :> Capture "handle" Handle
    :> End)
    NoQuery

type UpdateTeamProfile = Route
    Put
    (  Literal "teams"
    :> Capture "teamHandle" Handle
    :> Literal "profiles"
    :> Capture "gameHandle" Nickname
    :> End)
    NoQuery

type ViewPlayerProfilesByGame = Route
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
    :? Many "locations" Location
    :? Optional "weekdayFrom" Time
    :? Optional "weekdayTo" Time
    :? Optional "weekendFrom" Time
    :? Optional "weekendTo" Time
    :? Optional "microphone" HasMicrophone
    :? Many "platforms" Platform
    :? Optional "newOrReturning" NewOrReturning
    :? Rest "fields")

type ViewTeamProfilesByGame = Route
    Get
    (  Literal "profiles"
    :> Literal "by-handle"
    :> Capture "handle" Handle
    :> Literal "teams"
    :> End)
    (  Mandatory "page" ProfilePage
    :? Mandatory "timezone" Timezone
    :? Optional "ageFrom" Age
    :? Optional "ageTo" Age
    :? Many "languages" Language
    :? Many "locations" Location
    :? Optional "weekdayFrom" Time
    :? Optional "weekdayTo" Time
    :? Optional "weekendFrom" Time
    :? Optional "weekendTo" Time
    :? Optional "microphone" HasMicrophone
    :? Many "platforms" Platform
    :? Optional "newOrReturning" NewOrReturning
    :? Rest "fields")

type ProfileRoutes
    =    "addPlayerProfile"           := AddPlayerProfile
    :<|> "addTeamProfile"             := AddTeamProfile
    :<|> "updatePlayerProfile"        := UpdatePlayerProfile
    :<|> "updateTeamProfile"          := UpdateTeamProfile
    :<|> "viewPlayerProfilesByGame"   := ViewPlayerProfilesByGame
    :<|> "viewTeamProfilesByGame"     := ViewTeamProfilesByGame
