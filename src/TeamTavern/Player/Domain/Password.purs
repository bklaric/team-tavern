module TeamTavern.Player.Domain.Password where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import Data.Variant (Variant)
import Wrapped.String (TooShort, tooShort)
import Wrapped.Validated as Wrapped

newtype Password = Password String

derive instance newtypePassword :: Newtype Password _

type PasswordError = Variant (tooShort :: TooShort)

minPasswordLength :: Int
minPasswordLength = 8

create :: String -> Validated (NonEmptyList PasswordError) Password
create password =
    Wrapped.create identity [tooShort minPasswordLength] Password password
