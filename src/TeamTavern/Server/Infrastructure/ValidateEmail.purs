module TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail, validateEmail', toString) where

import Prelude

import Async (Async)
import Async.Validated (fromValidated)
import Data.Bifunctor.Label (labelMap)
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Validated.Label (ValidatedNelVari)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Wrapped.String (invalid, tooLong)
import Wrapped.Validated as Wrapped

newtype Email = Email String

emailRegex :: Regex
emailRegex = regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # unsafePartial fromRight

closeRow :: forall carrier rowTaking row. carrier (rowTaking row) -> carrier (rowTaking ())
closeRow = unsafeCoerce

validateEmail :: forall errors. String -> ValidatedNelVari (email :: Array String | errors) Email
validateEmail email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # Validated.labelMap (SProxy :: _ "email") \errors ->
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show (closeRow errors)
        ]

validateEmail' :: forall errors. String -> Async (Variant (email :: Array String | errors)) Email
validateEmail' email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # fromValidated
    # labelMap (SProxy :: _ "email") \errors ->
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show (closeRow errors)
        ]

toString :: Email -> String
toString (Email email) = email
