module TeamTavern.Server.Infrastructure.SendResponse where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Map (Map)
import Data.Map as Map
import Data.Variant (Variant)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar, lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (logError)

sendResponse
    :: ∀ responses
    .  String
    -> Async (TerrorVar responses) (Variant responses)
    -> (∀ left. Async left (Variant responses))
sendResponse heading =
    alwaysRight (\(Terror error _) -> error) identity
    <<< examineLeftWithEffect (logError heading)

lmapElaborateReferrer :: ∀ right error.
    Map String String -> Async (Terror error) right -> Async (Terror error) right
lmapElaborateReferrer headers =
    lmapElaborate ("Referrer: " <> (show $ Map.lookup "referer" headers))

lmapElaborateUserAgent :: ∀ right error.
    Map String String -> Async (Terror error) right -> Async (Terror error) right
lmapElaborateUserAgent headers =
    lmapElaborate ("User agent: " <> (show $ Map.lookup "user-agent" headers))
