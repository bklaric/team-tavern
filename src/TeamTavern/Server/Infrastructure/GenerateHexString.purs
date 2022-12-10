module TeamTavern.Server.Infrastructure.GenerateHexString where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Jarilo (internal__)
import Node.Buffer (toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

newtype ByteCount = ByteCount Int

derive instance Newtype ByteCount _

generateHexString :: forall errors. ByteCount -> Async (InternalTerror_ errors) String
generateHexString (ByteCount byteCount) = do
    bytes <- randomBytes byteCount
        # fromEitherCont
        # lmap \error -> Terror internal__
            [ "There has been an error generating a hex string: " <> print error ]
    string <- toString__ Hex bytes # fromEffect
    pure string
