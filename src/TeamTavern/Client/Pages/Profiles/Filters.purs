module TeamTavern.Client.Pages.Profile.Filters where


import Data.Maybe (Maybe)
import TeamTavern.Client.Components.Team.ProfileInputGroup (FieldValues)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)

type Filters =
    { organizations :: Array Organization
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , sizes :: Array Size
    , platforms :: Array Platform
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    }
