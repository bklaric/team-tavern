module TeamTavern.Server.Player.Register.ReadDto where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import Type.Proxy (Proxy(..))
import Yoga.JSON.Async (readJSON)

type ReadRegisterDtoError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readDto :: forall errors.
    Body -> Async (ReadRegisterDtoError errors) RegisterPlayer.RequestContent
readDto body = do
    content <- readBody body
    readJSON content
        # labelMap (Proxy :: _ "unreadableDto") { content, errors: _ }
