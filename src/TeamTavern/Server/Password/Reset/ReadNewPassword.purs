module TeamTavern.Server.Password.Reset.ReadNewPassword where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type Password = String

type Nonce = String

type NewPassword = { password :: Password, nonce :: Nonce }

type ReadNewPasswordError errors = Variant
    ( unreadableNewPassword ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readNewPassword :: forall errors.
    Body -> Async (ReadNewPasswordError errors) NewPassword
readNewPassword body = do
    content <- readBody body
    newPassword <- readJSON content # labelMap
        (SProxy :: SProxy "unreadableNewPassword") { content, errors: _ }
    pure newPassword
