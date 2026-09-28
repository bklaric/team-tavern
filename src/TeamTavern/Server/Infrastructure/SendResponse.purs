module TeamTavern.Server.Infrastructure.SendResponse where

import Prelude

import Async (Async, alwaysRight, examineLeftWithEffect)
import Data.Map (Map)
import Data.Map as Map
import Data.Variant (Variant, on)
import Jarilo.Router.Response (AppResponse)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar, lmapElaborate)
import TeamTavern.Server.Infrastructure.Log (logError)
import Type.Proxy (Proxy(..))

-- | Logs only an internal error. Every other status is an answer the client
-- | handles, and logging those buries the failures among signed-out visitors,
-- | taken nicknames and deleted posts.
sendResponse
    :: ∀ responses body
    .  String
    -> Async (TerrorVar (internal :: AppResponse body | responses)) (Variant (internal :: AppResponse body | responses))
    -> (∀ left. Async left (Variant (internal :: AppResponse body | responses)))
sendResponse heading =
    alwaysRight (\(Terror error _) -> error) identity
    <<< examineLeftWithEffect \terror@(Terror error _) ->
        error # on (Proxy :: _ "internal") (const $ logError heading terror) (const $ pure unit)

lmapElaborateReferrer :: ∀ right error.
    Map String String -> Async (Terror error) right -> Async (Terror error) right
lmapElaborateReferrer headers =
    lmapElaborate ("Referrer: " <> (show $ Map.lookup "referer" headers))

lmapElaborateUserAgent :: ∀ right error.
    Map String String -> Async (Terror error) right -> Async (Terror error) right
lmapElaborateUserAgent headers =
    lmapElaborate ("User agent: " <> (show $ Map.lookup "user-agent" headers))
