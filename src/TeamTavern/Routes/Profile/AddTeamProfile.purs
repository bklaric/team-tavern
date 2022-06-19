module TeamTavern.Routes.Profile.AddTeamProfile where

import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)
import TeamTavern.Routes.Shared.Types (Handle)
import Type (type ($))

type AddTeamProfile = FullRoute
    Post
    (  Literal "teams"
    :> Capture "teamHandle" Handle
    :> Literal "profiles"
    :> Capture "gameHandle" Handle
    :> End)
    NoQuery

type RouteParams = { teamHandle :: Handle, gameHandle :: Handle }

type RequestContentFieldValue =
    { fieldKey :: String
    , optionKeys :: Array String
    }

type RequestContentProfile =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: Array RequestContentFieldValue
    , newOrReturning :: Boolean
    , about :: String
    , ambitions :: String
    }

type RequestContent =
    { details :: RequestContentProfile
    , contacts :: TeamContacts
    }

type BadContent = Array $ Variant
    ( profile :: Array $ Variant
        ( platforms :: Array String
        , about :: Array String
        , ambitions :: Array String
        )
    , contacts :: Array TeamContactsError
    )
