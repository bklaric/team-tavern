module TeamTavern.Server.Player.Domain.Password where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Newtype (class Newtype)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import Type.Proxy (Proxy(..))
import Wrapped.String (TooShort, tooShort)
import Wrapped.Validated as Wrapped

newtype Password = Password String

derive instance newtypePassword :: Newtype Password _

type PasswordError = Variant (tooShort :: TooShort)

type PasswordErrors = NonEmptyArray PasswordError

minPasswordLength :: Int
minPasswordLength = 8

validatePassword :: forall errors.
    String -> ValidatedTerrorNeaVar (password :: {} | errors) Password
validatePassword password =
    Wrapped.create identity [tooShort minPasswordLength] Password password
    # Validated.lmap \(errors :: PasswordErrors) -> Terror
        (Nea.singleton $ inj (Proxy :: _ "password") {})
        [ "Registration password is invalid: " <> show errors ]
