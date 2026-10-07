module TeamTavern.Server.Infrastructure.ValidateEmail (Email, VouchedEmail, validateEmail, validateEmail', toString, vouchedEmail) where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), isJust)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import Jarilo (badRequest_)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Wrapped.String (TooLong, Invalid, invalid, tooLong)
import Wrapped.Validated as Wrapped

newtype Email = Email String

derive newtype instance Show Email

emailRegex :: Regex
emailRegex = regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # fromRight undefined

type EmailError = Variant (invalid :: Invalid, tooLong :: TooLong)

validateEmail :: forall errors. String -> ValidatedTerrorNeaVar (email :: {} | errors) Email
validateEmail email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # Validated.lmap \(errors :: NonEmptyArray EmailError) -> Terror
        (Nea.singleton $ inj (Proxy :: _ "email") {})
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show errors
        ]

validateEmail' :: ∀ other errors. String -> Async (BadRequestTerror (Variant (email :: {} | other)) errors) Email
validateEmail' email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254] Email email
    # AsyncVal.fromValidated
    # lmap \errors -> Terror
        (badRequest_ $ inj (Proxy :: _ "email") {})
        [ "Error validating email: " <> email
        , "Failed with following errors: " <> show (errors :: NonEmptyArray EmailError)
        ]

toString :: Email -> String
toString (Email email) = email

-- | An address a site the player signs in with gave, and whether that site
-- | verified it, which confirms it here.
type VouchedEmail = { email :: String, confirmed :: Boolean }

-- | The address a site gave, if any, with whether it says it verified it. The
-- | site vouches for owning the address, not for its shape, so it passes the
-- | same validation as an address a player types in.
vouchedEmail :: Maybe String -> Maybe Boolean -> Maybe VouchedEmail
vouchedEmail email verified = do
    given <- email
    valid <- Validated.hush (validateEmail given :: ValidatedTerrorNeaVar (email :: {}) Email)
    pure { email: toString valid, confirmed: verified == Just true }
