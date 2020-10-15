module TeamTavern.Server.Infrastructure.ReadJsonBody where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Perun.Request.Body (Body)
import Simple.JSON (class ReadForeign)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

readJsonBody :: forall errors result. ReadForeign result =>
    Body -> Async (Variant (client :: Array String | errors)) result
readJsonBody body = do
    content <- readBody body
    readJSON content # labelMap (SProxy :: SProxy "client") \errors ->
        [ "Error reading request body: " <> show errors ]
