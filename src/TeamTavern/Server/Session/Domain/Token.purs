module TeamTavern.Server.Session.Domain.Token where

import Prelude

import Async (Async)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import TeamTavern.Server.Infrastructure.Error (InternalError_)
import TeamTavern.Server.Infrastructure.GenerateHexString (ByteCount(..), generateHexString)

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

derive instance genericToken :: Generic Token _

instance showToken :: Show Token where show = genericShow

tokenByteCount :: ByteCount
tokenByteCount = ByteCount 20

generate :: forall errors. Async (InternalError_ errors) Token
generate = generateHexString tokenByteCount <#> Token
