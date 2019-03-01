module TeamTavern.Player.Register.GenerateNonce
    (Nonce, generateNonce, unNonce) where

import Async (Async)
import TeamTavern.Infrastructure.GenerateHexString (class HexString, GenerateHexStringError, generateHexString)

newtype Nonce = Nonce String

instance hexStringNonce :: HexString Nonce where
    fromHexString = Nonce

nonceCharCount :: Int
nonceCharCount = 20

generateNonce :: forall errors. Async (GenerateHexStringError errors) Nonce
generateNonce = generateHexString nonceCharCount

unNonce :: Nonce -> String
unNonce (Nonce nonce) = nonce
