module TeamTavern.Player.Domain.Nonce (Nonce, NonceError, nonceCharCount, create) where

import Prelude

import Data.Either (Either)
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

newtype Nonce = Nonce String

derive instance newtypeNonce :: Newtype Nonce _

type NonceError = Variant
    ( notExactlyLong :: NotExactlyLong
    , notHex :: NotHex
    )

nonceCharCount :: CharCount
nonceCharCount = CharCount.create 20 # unsafePartial fromJust

create :: String -> Either (NonEmptyList NonceError) Nonce
create token =
    Wrapped.create
        (trim >>> toLower)
        [notExactlyLong (unwrap nonceCharCount), notHex]
        Nonce token
