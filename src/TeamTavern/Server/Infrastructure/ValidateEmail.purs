module TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail, validateEmail', toString) where

import Prelude

import Async (Async)
import Async.Validated (fromValidated)
import Data.Bifunctor.Label (labelMap)
import Data.Either (fromRight)
import Data.List.Types (NonEmptyList)
import Data.Maybe (isJust)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Partial.Unsafe (unsafePartial)
import Wrapped.String (TooLong, Invalid, invalid, tooLong)
import Wrapped.Validated as Wrapped

newtype Email = Email String

emailRegex :: Regex
emailRegex = regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # unsafePartial fromRight

type EmailError = Variant (invalid :: Invalid, tooLong :: TooLong )

validateEmail :: forall errors. String -> ValidatedVariants (email :: Array String | errors) Email
validateEmail email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # Validated.labelMap (SProxy :: _ "email") \errors ->
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show (errors :: NonEmptyList EmailError)
        ]

validateEmail' :: forall errors. String -> Async (Variant (email :: Array String | errors)) Email
validateEmail' email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # fromValidated
    # labelMap (SProxy :: _ "email") \errors ->
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show (errors :: NonEmptyList EmailError)
        ]

toString :: Email -> String
toString (Email email) = email
