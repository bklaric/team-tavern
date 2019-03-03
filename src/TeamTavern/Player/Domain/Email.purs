module TeamTavern.Player.Domain.Email where

import Prelude

import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (isJust)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Validated (Validated)
import Data.Variant (Variant)
import Partial.Unsafe (unsafePartial)
import Wrapped.String (Invalid, TooLong, invalid, tooLong)
import Wrapped.Validated as Wrapped

newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _

derive instance genericEmail :: Generic Email _

instance showEmail :: Show Email where
    show = genericShow

type EmailError = Variant (invalid :: Invalid, tooLong :: TooLong)

emailRegex :: Regex
emailRegex =
    regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # unsafePartial fromRight

create :: String -> Validated (NonEmptyList EmailError) Email
create email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254]
        Email email
