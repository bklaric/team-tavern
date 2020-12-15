module TeamTavern.Server.Player.Register.ReadDto where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type RegisterDto =
    { nickname :: String
    , password :: String
    }

type ReadRegisterDtoError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readDto :: forall errors.
    Body -> Async (ReadRegisterDtoError errors) RegisterDto
readDto body = do
    content <- readBody body
    readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
