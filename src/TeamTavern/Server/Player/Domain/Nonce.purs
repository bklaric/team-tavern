module TeamTavern.Server.Player.Domain.Nonce where

import Prelude

import Async (Async)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import TeamTavern.Server.Infrastructure.GenerateHexString (ByteCount(..), GenerateHexStringError, generateHexString)

newtype Nonce = Nonce String

derive instance newtypeNonce :: Newtype Nonce _

derive instance genericNonce :: Generic Nonce _

instance showNonce :: Show Nonce where show = genericShow

-- 10 bytes = 20 hex characters.
nonceByteCount :: ByteCount
nonceByteCount = ByteCount 10

generate :: forall errors. Async (GenerateHexStringError errors) Nonce
generate = generateHexString nonceByteCount <#> Nonce
