module TeamTavern.Server.Infrastructure.SendResponse where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Variant (Variant)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (logError)

sendResponse
    :: forall responses
    .  String
    -> Async (TerrorVar responses) (Variant responses)
    -> (forall left. Async left (Variant responses))
sendResponse heading =
    alwaysRight (\(Terror error _) -> error) identity
    <<< examineLeftWithEffect (logError heading)
