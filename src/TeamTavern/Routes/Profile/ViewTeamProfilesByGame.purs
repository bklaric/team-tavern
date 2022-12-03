module TeamTavern.Routes.Profile.ViewTeamProfilesByGame where

import Prelude

import Data.Maybe (Maybe, maybe)
import Jarilo (type (&), type (/), type (==>), Capture, Get, Literal, Mandatory, Many, OkJson, Optional, Rest)
import TeamTavern.Routes.Profile.Shared (ProfilePage, bundleFields)
import TeamTavern.Routes.Shared.Filters (Age, Filters, HasMicrophone, Language, Location, NewOrReturning, Time)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamBase (TeamBaseRow)
import TeamTavern.Routes.Shared.TeamContacts (TeamContactsRow)
import TeamTavern.Routes.Shared.TeamDetails (TeamDetailsRow)
import TeamTavern.Routes.Shared.TeamProfile (TeamProfileRow)
import TeamTavern.Routes.Shared.Types (Handle, Timezone)
import Type.Row (type (+))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

type ViewTeamProfilesByGame =
    Get
    ( Literal "games"
    / Capture "handle" Handle
    / Literal "teams")
    (  Mandatory "page" ProfilePage
    & Mandatory "timezone" Timezone
    & Many "organization" Organization
    & Optional "ageFrom" Age
    & Optional "ageTo" Age
    & Many "language" Language
    & Many "location" Location
    & Optional "weekdayFrom" Time
    & Optional "weekdayTo" Time
    & Optional "weekendFrom" Time
    & Optional "weekendTo" Time
    & Optional "microphone" HasMicrophone
    & Many "size" Size
    & Many "platform" Platform
    & Optional "newOrReturning" NewOrReturning
    & Rest "fields")
    ==> OkJson OkContent

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

type OkContentProfiles = Record (TeamBaseRow + TeamContactsRow + TeamDetailsRow + TeamProfileRow + ())

type OkContent =
    { profiles :: Array OkContentProfiles
    , count :: Int
    }
