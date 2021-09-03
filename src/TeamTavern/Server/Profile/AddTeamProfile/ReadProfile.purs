module TeamTavern.Server.Profile.AddTeamProfile.ReadProfile where

import Async (Async)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
import TeamTavern.Routes.Shared.Team (Contacts)
import TeamTavern.Server.Infrastructure.Error (ClientError)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)

type FieldValue =
    { fieldKey :: String
    , optionKeys :: Array String
    }

type Profile =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , about :: String
    }

type RequestContent =
    { details :: Profile
    , contacts :: Contacts
    }

readProfile :: forall errors. Body -> Async (ClientError errors) RequestContent
readProfile = readJsonBody
