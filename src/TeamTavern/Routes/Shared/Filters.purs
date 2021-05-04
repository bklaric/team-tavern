module TeamTavern.Routes.Shared.Filters where

import Data.Maybe (Maybe)
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)

type Age = Int

type Language = String

type Location = String

type Time = String

type HasMicrophone = Boolean

type Field = { fieldKey :: String, optionKeys :: Array String }

type NewOrReturning = Boolean

type Filters =
    { organizations :: Array Organization
    , ageFrom :: Maybe Age
    , ageTo :: Maybe Age
    , languages :: Array Language
    , locations :: Array Location
    , weekdayFrom :: Maybe Time
    , weekdayTo :: Maybe Time
    , weekendFrom :: Maybe Time
    , weekendTo :: Maybe Time
    , microphone :: HasMicrophone
    , sizes :: Array Size
    , platforms :: Array Platform
    , fields :: Array Field
    , newOrReturning :: NewOrReturning
    }
