module TeamTavern.Routes.Shared.TeamDetails where

import Data.Maybe (Maybe)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)

type TeamDetailsRow fields =
    ( organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayOnline :: Maybe { from :: String, to :: String }
    , weekendOnline :: Maybe { from :: String, to :: String }
    | fields
    )

type TeamDetailsOpen fields = Record (TeamDetailsRow fields)

type TeamDetails = TeamDetailsOpen ()
