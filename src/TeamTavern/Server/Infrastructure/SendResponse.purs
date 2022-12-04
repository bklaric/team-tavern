module TeamTavern.Server.Infrastructure.SendResponse where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Variant (Variant)
import TeamTavern.Server.Infrastructure.Error (TavernError, errorResponse)
import TeamTavern.Server.Infrastructure.Log (logError)

sendResponse
    :: forall responses
    .  String
    -> Async (TavernError responses) (Variant responses)
    -> (forall left. Async left (Variant responses))
sendResponse heading =
    alwaysRight errorResponse identity
    <<< examineLeftWithEffect (logError heading)
