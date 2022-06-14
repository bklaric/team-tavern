module TeamTavern.Server.Session.Start.ReadModel where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.String (trim)
import Type.Proxy (Proxy(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Yoga.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type StartDto =
    { nickname :: String
    , password :: String
    }

type StartModel = StartDto

type ReadNonceError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readModel :: forall errors. Body -> Async (ReadNonceError errors) StartModel
readModel body = do
    content <- readBody body
    { nickname, password } :: StartDto <- readJSON content
        # labelMap (Proxy :: _ "unreadableDto") { content, errors: _ }
    pure { nickname: trim nickname, password }
