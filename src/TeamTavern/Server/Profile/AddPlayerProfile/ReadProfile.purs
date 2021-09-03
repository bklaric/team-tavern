module TeamTavern.Server.Profile.AddPlayerProfile.ReadProfile where

import Async (Async)
import Data.Maybe (Maybe)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Shared.Platform (Platform)
import TeamTavern.Routes.Shared.Player (Contacts)
import TeamTavern.Server.Infrastructure.Error (ClientError)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)

type FieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type Profile =
    { platform :: Platform
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , about :: String
    , ambitions :: String
    }

type RequestContent =
    { details :: Profile
    , contacts :: Contacts
    }

readProfile :: forall errors. Body -> Async (ClientError errors) RequestContent
readProfile = readJsonBody
