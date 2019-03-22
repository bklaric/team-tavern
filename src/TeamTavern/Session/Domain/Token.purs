module TeamTavern.Session.Domain.Token where

import Prelude

import Async (Async)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import TeamTavern.Infrastructure.GenerateHexString (ByteCount(..), GenerateHexStringError, generateHexString)

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where show = genericShow

tokenByteCount :: ByteCount
tokenByteCount = ByteCount 20

generate :: forall errors. Async (GenerateHexStringError errors) Token
generate = generateHexString tokenByteCount <#> Token
