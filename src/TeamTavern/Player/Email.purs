module TeamTavern.Player.Email (Email, EmailErrors, Invalid, create) where

import Prelude

import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Validation.Semigroup (V)
import Data.Variant (SProxy(..), Variant, inj)
import Partial.Unsafe (unsafePartial)
import Wrapped as Wrapped
import Wrapped.String (TooLong, tooLong)

newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _

type Invalid = { original :: String }

emailRegex :: Regex
emailRegex =
    regex """/^[^\s@]+@[^\s@]+\.[^\s@]+$/""" unicode # unsafePartial fromRight

invalid :: forall errors.
    String -> Maybe (Variant (invalid :: Invalid | errors))
invalid email =
    case match emailRegex email of
    Just [Just matchedEmail] -> Nothing
    _ -> Just $ inj (SProxy :: SProxy "invalid") { original: email }

type EmailErrors = Array (Variant (tooLong :: TooLong, invalid :: Invalid))

create :: String -> V EmailErrors Email
create email = Wrapped.create [tooLong 254, invalid] trim Email email
