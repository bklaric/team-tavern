module TeamTavern.Server.Session.Domain.Token where

import Prelude

import Async (Async)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import TeamTavern.Server.Infrastructure.GenerateHexString (ByteCount(..), generateHexString)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

newtype Token = Token String

derive instance Newtype Token _

derive instance Generic Token _

instance Show Token where show = genericShow

tokenByteCount :: ByteCount
tokenByteCount = ByteCount 20

generate :: forall errors. Async (InternalTerror_ errors) Token
generate = generateHexString tokenByteCount <#> Token
