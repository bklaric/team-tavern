module TeamTavern.Player.Email (Email, EmailError, Invalid, create) where

import Prelude

import Data.Either (fromRight)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Variant (SProxy(..), Variant, inj)
import Partial.Unsafe (unsafePartial)
import Validated (Validated)
import Wrapped.String (TooLong, tooLong)
import Wrapped.Validated as Wrapped

newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _

type Invalid = { original :: String }

emailRegex :: Regex
emailRegex =
    regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # unsafePartial fromRight

invalid :: forall errors.
    String -> Maybe (Variant (invalid :: Invalid | errors))
invalid email =
    case match emailRegex email of
    Just _ -> Nothing
    Nothing -> Just $ inj (SProxy :: SProxy "invalid") { original: email }

type EmailError = Variant (tooLong :: TooLong, invalid :: Invalid)

create :: String -> Validated (NonEmptyList EmailError) Email
create email = Wrapped.create trim [invalid, tooLong 254] Email email
