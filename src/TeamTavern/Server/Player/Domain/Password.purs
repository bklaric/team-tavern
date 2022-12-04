module TeamTavern.Server.Player.Domain.Password where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import TeamTavern.Server.Infrastructure.Error (TavernErrorMany(..), ValidatedTavern)
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
    String -> ValidatedTavern (password :: {} | errors) Password
validatePassword password =
    Wrapped.create identity [tooShort minPasswordLength] Password password
    # Validated.lmap \(errors :: PasswordErrors) -> TavernErrorMany
        (singleton $ inj (Proxy :: _ "password") {})
        [ "Registration password is invalid: " <> show errors ]
