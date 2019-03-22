module TeamTavern.Player.Domain.Nickname where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated)
import Data.Variant (Variant)
import Jarilo.FromComponent (class FromComponent)
import Wrapped.String (Empty, NotAsciiAlphaNum, TooLong, empty, notAsciiAlphaNum, tooLong)
import Wrapped.Validated as Wrapped

newtype Nickname = Nickname String

derive instance newtypeNickname :: Newtype Nickname _

derive newtype instance showNickname :: Show Nickname

derive newtype instance fromComponentNickname :: FromComponent Nickname

type NicknameError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNum :: NotAsciiAlphaNum
    )

maxLength :: Int
maxLength = 40

create :: String -> Validated (NonEmptyList NicknameError) Nickname
create nickname =
    Wrapped.create trim [empty, tooLong maxLength, notAsciiAlphaNum]
    Nickname nickname
