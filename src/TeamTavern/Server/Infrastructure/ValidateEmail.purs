module TeamTavern.Server.Infrastructure.ValidateEmail (Email, validateEmail, toString) where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Either (fromRight)
import Data.Maybe (isJust)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Variant (Variant)
import Jarilo (internal__)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Undefined (undefined)
import Wrapped.String (TooLong, Invalid, invalid, tooLong)
import Wrapped.Validated as Wrapped

newtype Email = Email String

emailRegex :: Regex
emailRegex = regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # fromRight undefined

type EmailError = Variant (invalid :: Invalid, tooLong :: TooLong )

validateEmail :: forall errors. String -> Async (InternalTerror_ errors) Email
validateEmail email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # AsyncVal.fromValidated
    # lmap \errors -> Terror internal__
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show (errors :: NonEmptyArray EmailError)
        ]

toString :: Email -> String
toString (Email email) = email
