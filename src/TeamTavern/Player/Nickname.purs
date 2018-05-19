module TeamTavern.Player.Nickname (Nickname, NicknameErrors, create) where


import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Wrapped as Wrapped
import Wrapped.String (ContainsWhitespace, NotPrintable, TooLong, Empty, containsWhitespace, empty, notPrintable, tooLong)

newtype Nickname = Nickname String

derive instance newtypeNickname :: Newtype Nickname _

type NicknameErrors = Array (Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    , containsWhitespace :: ContainsWhitespace
    ))

create :: String -> V NicknameErrors Nickname
create nickname =
    Wrapped.create
        [empty, tooLong 40, notPrintable, containsWhitespace]
        trim Nickname nickname
