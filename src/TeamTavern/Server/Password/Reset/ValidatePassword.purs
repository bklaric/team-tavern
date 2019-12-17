module TeamTavern.Server.Password.Reset.ValidatePassword where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Server.Player.Domain.Password (Password, PasswordError)
import TeamTavern.Server.Player.Domain.Password as Password

type ValidatePasswordError errors = Variant
    ( invalidPassword ::
        { password :: String
        , errors :: NonEmptyList PasswordError
        }
    | errors )

validatePassword :: forall errors.
    String -> Async (ValidatePasswordError errors) Password
validatePassword password =
    Password.create password
    # Async.fromValidated
    # labelMap (SProxy :: SProxy "invalidPassword") { password, errors: _ }
