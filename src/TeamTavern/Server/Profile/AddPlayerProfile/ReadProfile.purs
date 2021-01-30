module TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile where

import Async (Async)
import Data.Maybe (Maybe)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Shared.ExternalIdIlk (ExternalIdIlk)
import TeamTavern.Server.Infrastructure.Error (ClientError)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)

type FieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type Profile =
    { externalIdIlk :: ExternalIdIlk
    , externalId :: String
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , ambitions :: String
    }

readProfile :: forall errors. Body -> Async (ClientError errors) Profile
readProfile = readJsonBody
