module TeamTavern.Server.Player.Domain.Password where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import Jarilo (badRequest_)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import Type.Proxy (Proxy(..))
import Wrapped.String (TooShort, tooShort)
import Wrapped.Validated as Wrapped

newtype Password = Password String

derive instance Newtype Password _

type PasswordError = Variant (tooShort :: TooShort)

type PasswordErrors = NonEmptyArray PasswordError

minPasswordLength :: Int
minPasswordLength = 8

validatePassword :: ∀ errors.
    String -> ValidatedTerrorNeaVar (password :: {} | errors) Password
validatePassword password =
    Wrapped.create identity [tooShort minPasswordLength] Password password
    # Validated.lmap \(errors :: PasswordErrors) -> Terror
        (Nea.singleton $ inj (Proxy :: _ "password") {})
        ["Password is invalid: " <> show errors]

validatePassword' :: String -> Async _ Password
validatePassword' password =
    Wrapped.create identity [tooShort minPasswordLength] Password password
    # AsyncVal.fromValidated
    # lmap \(errors :: PasswordErrors) -> Terror
        (badRequest_ $ inj (Proxy :: _ "password") {})
        ["Password is invalid: " <> show errors]
