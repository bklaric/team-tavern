module TeamTavern.Player.Domain.Nonce
    (Nonce, NonceError, nonceCharCount, create, create') where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String (toLower, trim)
import Data.Validated (Validated, toEither)
import Data.Variant (Variant)
import Partial.Unsafe (unsafePartial)
import TeamTavern.Player.Domain.CharCount (CharCount)
import TeamTavern.Player.Domain.CharCount as CharCount
import Wrapped.String (NotExactlyLong, NotHex, notExactlyLong, notHex)
import Wrapped.Validated as Wrapped

newtype Nonce = Nonce String

derive instance newtypeNonce :: Newtype Nonce _

derive instance genericNonce :: Generic Nonce _

instance showNonce :: Show Nonce where
    show = genericShow

type NonceError = Variant
    ( notExactlyLong :: NotExactlyLong
    , notHex :: NotHex
    )

nonceCharCount :: CharCount
nonceCharCount = CharCount.create 20 # unsafePartial fromJust

create :: String -> Validated (NonEmptyList NonceError) Nonce
create token =
    Wrapped.create
        (trim >>> toLower)
        [notExactlyLong (unwrap nonceCharCount), notHex]
        Nonce token

create' :: String -> Either (NonEmptyList NonceError) Nonce
create' = create >>> toEither
