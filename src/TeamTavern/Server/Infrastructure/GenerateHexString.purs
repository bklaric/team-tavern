module TeamTavern.Server.Infrastructure.GenerateHexString where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Jarilo (internal__)
import Node.Buffer (toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import TeamTavern.Server.Infrastructure.Error (InternalError_, TavernError(..))
import TeamTavern.Server.Infrastructure.Log (print)

newtype ByteCount = ByteCount Int

derive instance newtypeByteCount :: Newtype ByteCount _

generateHexString :: forall errors. ByteCount -> Async (InternalError_ errors) String
generateHexString (ByteCount byteCount) = do
    bytes <- randomBytes byteCount
        # fromEitherCont
        # lmap \error -> TavernError internal__
            [ "There has been an error generating a hex string: " <> print error ]
    string <- toString__ Hex bytes # fromEffect
    pure string
