module TeamTavern.Routes.Profile.ViewPlayerProfilesByGame where

import Prelude

import Data.Maybe (Maybe)
import Jarilo.Method (Get)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (type (:?), Mandatory, Many, Optional, Rest)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import Prim.Row (class Lacks)
import Record as Record
import TeamTavern.Routes.Profile.Shared (ProfilePage)
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame (bundleTeamFilters)
import TeamTavern.Routes.Shared.Filters (Age, Filters, HasMicrophone, Language, Location, NewOrReturning, Time)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.PlayerBase (PlayerBaseRow)
import TeamTavern.Routes.Shared.PlayerContacts (PlayerContactsRow)
import TeamTavern.Routes.Shared.PlayerDetails (PlayerDetailsRow)
import TeamTavern.Routes.Shared.PlayerProfile (PlayerProfileRow)
import TeamTavern.Routes.Shared.Types (Handle, Timezone)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import URI.Extra.QueryPairs (Key, QueryPairs, Value)

type ViewPlayerProfilesByGame = FullRoute
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
    $ Record.insert (Proxy :: _ "organization") []
    $ Record.insert (Proxy :: _ "size") []
    $ filters

type OkContentProfiles = Record (PlayerBaseRow + PlayerContactsRow + PlayerDetailsRow + PlayerProfileRow + ())

type OkContent =
    { profiles :: Array OkContentProfiles
    , count :: Int
    }