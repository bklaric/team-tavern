module TeamTavern.Player.Domain.Token (Token, TokenError, tokenCharCount, create) where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (toLower, trim)
import Data.Variant (Variant)
import Wrapped as Wrapped
import Wrapped.String (NotExactlyLong, NotHex, notExactlyLong, notHex)

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

type TokenError = Variant
    ( notExactlyLong :: NotExactlyLong
    , notHex :: NotHex
    )

tokenCharCount :: Int
tokenCharCount = 40

create :: String -> Either (NonEmptyList TokenError) Token
create token =
    Wrapped.create
        (trim >>> toLower)
        [notExactlyLong tokenCharCount, notHex]
        Token token
