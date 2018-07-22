module TeamTavern.Player.Domain.Nonce (Nonce, NonceError, nonceCharCount, create) where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (toLower, trim)
import Data.Variant (Variant)
import Wrapped as Wrapped
import Wrapped.String (NotExactlyLong, NotHex, notExactlyLong, notHex)

newtype Nonce = Nonce String

derive instance newtypeNonce :: Newtype Nonce _

type NonceError = Variant
    ( notExactlyLong :: NotExactlyLong
    , notHex :: NotHex
    )

nonceCharCount :: Int
nonceCharCount = 20

create :: String -> Either (NonEmptyList NonceError) Nonce
create token =
    Wrapped.create
        (trim >>> toLower)
        [notExactlyLong nonceCharCount, notHex]
        Nonce token
