module TeamTavern.Routes.Profile.AddTeamProfile where

import Data.Variant (Variant)
import Jarilo.Types (Post)
import Jarilo.Types (type (:>), Capture, Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, NoContent)
import Jarilo.Types (FullRoute)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.TeamContacts (TeamContacts, TeamContactsError)
import TeamTavern.Routes.Shared.Types (Handle)
import Type.Function (type ($))

type AddTeamProfile = FullRoute
    (Post RequestContent)
    (  Literal "teams"
    :> Capture "teamHandle" Handle
    :> Literal "profiles"
    :> Capture "gameHandle" Handle)
    NoQuery
    (NoContent :! BadRequest BadContent)

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
