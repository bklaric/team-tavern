module TeamTavern.Server.Infrastructure.GenerateNonce where

import Prelude

import Async (Async)
import TeamTavern.Server.Infrastructure.GenerateHexString (ByteCount(..), generateHexString)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

newtype Nonce = Nonce String

derive newtype instance Show Nonce

toString :: Nonce -> String
toString (Nonce nonce) = nonce

-- 10 bytes = 20 hex characters.
nonceByteCount :: ByteCount
nonceByteCount = ByteCount 10

generateNonce :: forall errors. Async (InternalTerror_ errors) Nonce
generateNonce = generateHexString nonceByteCount <#> Nonce
