module TeamTavern.Player.Nickname (Nickname, NicknameErrors, create) where


import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validation.Semigroup (V)
import Data.Variant (Variant)
import Wrapped as Wrapped
import Wrapped.String (ContainsWhitespace, NonPrintable, TooLong, containsWhitespace, notPrintable, tooLong)

newtype Nickname = Nickname String

derive instance newtypeNickname :: Newtype Nickname _

type NicknameErrors = Array (Variant
    ( tooLong :: TooLong
    , notPrintable :: NonPrintable
    , containsWhitespace :: ContainsWhitespace
    ))

create :: String -> V NicknameErrors Nickname
create nickname =
    Wrapped.create
        [tooLong 40, notPrintable, containsWhitespace] trim Nickname nickname
