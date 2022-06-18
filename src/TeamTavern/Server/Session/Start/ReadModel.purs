module TeamTavern.Server.Session.Start.ReadModel where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.String (trim)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Session.StartSession as StartSession
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (readJSON)

type ReadNonceError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readModel :: forall errors. Body -> Async (ReadNonceError errors) StartSession.RequestContent
readModel body = do
    content <- readBody body
    { nickname, password } :: StartSession.RequestContent <- readJSON content
        # labelMap (Proxy :: _ "unreadableDto") { content, errors: _ }
    pure { nickname: trim nickname, password }
