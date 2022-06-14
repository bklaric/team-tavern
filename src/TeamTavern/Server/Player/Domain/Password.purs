module TeamTavern.Server.Player.Domain.Password where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import Type.Proxy (Proxy(..))
import Wrapped.String (TooShort, tooShort)
import Wrapped.Validated as Wrapped

newtype Password = Password String

derive instance newtypePassword :: Newtype Password _

type PasswordError = Variant (tooShort :: TooShort)

type PasswordErrors = NonEmptyList PasswordError

minPasswordLength :: Int
minPasswordLength = 8

validatePassword :: forall errors.
    String -> ValidatedVariants (password :: Array String | errors) Password
validatePassword password =
    Wrapped.create identity [tooShort minPasswordLength] Password password
    # Validated.labelMap (Proxy :: _ "password") \(errors :: PasswordErrors) ->
        [ "Registration password is invalid: " <> show errors ]
