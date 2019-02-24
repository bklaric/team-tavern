module TeamTavern.Player.Register.GenerateNonce
    (Nonce, GenerateNonceError, generateNonce, unNonce) where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant)
import Node.Buffer (toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)

newtype Nonce = Nonce String

type GenerateNonceError errors = Variant (randomError :: Error | errors)

-- 10 bytes = 20 hex characters.
nonceByteCount :: Int
nonceByteCount = 10

generateNonce :: forall errors. Async (GenerateNonceError errors) Nonce
generateNonce = label (SProxy :: SProxy "randomError") do
    bytes <- randomBytes nonceByteCount # fromEitherCont
    nonce <- toString__ Hex bytes # fromEffect
    pure $ Nonce nonce

unNonce :: Nonce -> String
unNonce (Nonce nonce) = nonce
