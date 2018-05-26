module TeamTavern.Player.Nickname (Nickname, NicknameErrors, create) where


import Data.List (List)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Variant (Variant)
import Validated (Validated)
import Wrapped.String (ContainsWhitespace, NotPrintable, TooLong, Empty, containsWhitespace, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Nickname = Nickname String

derive instance newtypeNickname :: Newtype Nickname _

type NicknameErrors = List (Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    , containsWhitespace :: ContainsWhitespace
    ))

create :: String -> Validated NicknameErrors Nickname
create nickname =
    Wrapped.create
        trim [empty, tooLong 40, notPrintable, containsWhitespace]
        Nickname nickname
