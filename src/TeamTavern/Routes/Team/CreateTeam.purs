module TeamTavern.Routes.Team.CreateTeam where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Jarilo (type (!), type (==>), BadRequestJson, Internal_, Literal, OkJson, PostJson_, NotAuthorized_)
import TeamTavern.Routes.Shared.Organization (OrganizationNW)
import TeamTavern.Routes.Shared.TeamBase (TeamError)

type CreateTeam =
    PostJson_
    (Literal "teams")
    RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent ! NotAuthorized_ ! Internal_

type RequestContent =
    { organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }

type OkContent =
    { id :: Int
    , handle :: String
    }

type BadContent = NonEmptyArray TeamError
