module TeamTavern.Server.Profile.Routes where

import Prelude

import Data.Array (foldl, mapMaybe)
import Data.Maybe (Maybe, maybe)
import Data.MultiMap as MultiMap
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (type (:?), Mandatory, Many, NoQuery, Optional, Rest)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)
import Prim.Row (class Lacks)
import Record as Record
import TeamTavern.Routes.Shared.Filters (Age, Filters, HasMicrophone, Language, Location, NewOrReturning, Time, Field)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.Timezone (Timezone)
import TeamTavern.Routes.Shared.ViewPlayerProfile (ViewPlayerProfile)
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value, keyToString, valueToString)

type ProfilePage = Int

bundleFields :: QueryPairs Key Value -> Array Field
bundleFields (QueryPairs filters) = let
    preparedField fieldKey optionKey = let
        preparedFieldKey = keyToString fieldKey
        preparedOptionKey = valueToString optionKey
        in
        Tuple preparedFieldKey preparedOptionKey
    preparedFields =
        filters # mapMaybe \(Tuple fieldKey optionKey') ->
            optionKey' <#> \optionKey -> preparedField fieldKey optionKey
    groupedFields = preparedFields
        # foldl (\groupedFiltersSoFar (Tuple fieldKey optionKey) ->
            MultiMap.insertOrAppend' fieldKey optionKey groupedFiltersSoFar)
            MultiMap.empty
    in
    groupedFields # MultiMap.toUnfoldable' <#> \(Tuple fieldKey optionKeys) ->
        { fieldKey, optionKeys }

bundlePlayerFilters :: forall other. Lacks "organization" other => Lacks "size" other =>
    { ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , language :: Array String
    , location :: Array String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , microphone :: Maybe Boolean
    , platform :: Array Platform
    , fields :: QueryPairs Key Value
    , newOrReturning :: Maybe Boolean
    | other }
    -> Filters
bundlePlayerFilters filters =
    bundleTeamFilters
    $ Record.insert (SProxy :: _ "organization") []
    $ Record.insert (SProxy :: _ "size") []
    $ filters

bundleTeamFilters :: forall other.
    { organization :: Array Organization
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , language :: Array String
    , location :: Array String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , microphone :: Maybe Boolean
    , size :: Array Size
    , platform :: Array Platform
    , fields :: QueryPairs Key Value
    , newOrReturning :: Maybe Boolean
    | other }
    -> Filters
bundleTeamFilters filters =
    { organizations: filters.organization
    , ageFrom: filters.ageFrom
    , ageTo: filters.ageTo
    , languages: filters.language
    , locations: filters.location
    , weekdayFrom: filters.weekdayFrom
    , weekdayTo: filters.weekdayTo
    , weekendFrom: filters.weekendFrom
    , weekendTo: filters.weekendTo
    , microphone: maybe false identity filters.microphone
    , sizes: filters.size
    , platforms: filters.platform
    , fields: bundleFields filters.fields
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
    (  Literal "games"
    :> Capture "handle" Handle
    :> Literal "players"
    :> End)
    (  Mandatory "page" ProfilePage
    :? Mandatory "timezone" Timezone
    :? Optional "ageFrom" Age
    :? Optional "ageTo" Age
    :? Many "language" Language
    :? Many "location" Location
    :? Optional "weekdayFrom" Time
    :? Optional "weekdayTo" Time
    :? Optional "weekendFrom" Time
    :? Optional "weekendTo" Time
    :? Optional "microphone" HasMicrophone
    :? Many "platform" Platform
    :? Optional "newOrReturning" NewOrReturning
    :? Rest "fields")

type ViewTeamProfilesByGame = Route
    Get
    (  Literal "games"
    :> Capture "handle" Handle
    :> Literal "teams"
    :> End)
    (  Mandatory "page" ProfilePage
    :? Mandatory "timezone" Timezone
    :? Many "organization" Organization
    :? Optional "ageFrom" Age
    :? Optional "ageTo" Age
    :? Many "language" Language
    :? Many "location" Location
    :? Optional "weekdayFrom" Time
    :? Optional "weekdayTo" Time
    :? Optional "weekendFrom" Time
    :? Optional "weekendTo" Time
    :? Optional "microphone" HasMicrophone
    :? Many "size" Size
    :? Many "platform" Platform
    :? Optional "newOrReturning" NewOrReturning
    :? Rest "fields")

type ProfileRoutes
    =    "addPlayerProfile"         := AddPlayerProfile
    :<|> "addTeamProfile"           := AddTeamProfile
    :<|> "updatePlayerProfile"      := UpdatePlayerProfile
    :<|> "updateTeamProfile"        := UpdateTeamProfile
    :<|> "viewPlayerProfilesByGame" := ViewPlayerProfilesByGame
    :<|> "viewTeamProfilesByGame"   := ViewTeamProfilesByGame
    :<|> "viewPlayerProfile"        := ViewPlayerProfile
