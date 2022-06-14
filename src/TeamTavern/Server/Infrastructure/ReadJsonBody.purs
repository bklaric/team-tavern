module TeamTavern.Server.Infrastructure.ReadJsonBody where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Type.Proxy (Proxy(..))
import Data.Variant (Variant)
import Perun.Request.Body (Body)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

readJsonBody :: forall errors result. ReadForeign result =>
    Body -> Async (Variant (client :: Array String | errors)) result
readJsonBody body = do
    content <- readBody body
    readJSON content # labelMap (Proxy :: _ "client") \errors ->
        [ "Error reading request body: " <> show errors ]
