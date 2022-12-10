module TeamTavern.Server.Infrastructure.SendResponse where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Variant (Variant)
import Tasync (Tasync, getRequest)
import Tasync as Tasync
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Log (logError)

sendResponse
    ∷  ∀ responses
    .  String
    -> Async (TerrorVar responses) (Variant responses)
    -> (∀ left. Async left (Variant responses))
sendResponse heading =
    alwaysRight (\(Terror error _) -> error) identity
    <<< examineLeftWithEffect (logError heading)

-- sendResponse tasync = do
--     request <- getRequest
--     tasync
--         # Tasync.examineLeftWithEffect (logError request)
--         # Tasync.alwaysRight (\(Terror error _) -> error) identity
