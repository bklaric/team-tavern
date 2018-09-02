module TeamTavern.Player.Domain.Token (Token, TokenError, tokenCharCount, create) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String (toLower, trim)
import Data.Variant (Variant)
import Partial.Unsafe (unsafePartial)
import TeamTavern.Player.Domain.CharCount (CharCount)
import TeamTavern.Player.Domain.CharCount as CharCount
import Wrapped as Wrapped
import Wrapped.String (NotExactlyLong, NotHex, notExactlyLong, notHex)

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where
    show = genericShow

type TokenError = Variant
    ( notExactlyLong :: NotExactlyLong
    , notHex :: NotHex
    )

tokenCharCount :: CharCount
tokenCharCount = CharCount.create 40 # unsafePartial fromJust

create :: String -> Either (NonEmptyList TokenError) Token
create token =
    Wrapped.create
        (trim >>> toLower)
        [notExactlyLong (unwrap tokenCharCount), notHex]
        Token token
