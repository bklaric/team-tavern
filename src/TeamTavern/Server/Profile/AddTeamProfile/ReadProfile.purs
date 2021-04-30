module TeamTavern.Server.Profile.AddTeamProfile.ReadProfile where

import Async (Async)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Size (Size)
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
    , ambitions :: String
    }

readProfile :: forall errors. Body -> Async (ClientError errors) Profile
readProfile = readJsonBody
