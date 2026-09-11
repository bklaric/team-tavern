module TeamTavern.Server.Infrastructure.GenerateHexString where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Jarilo (internal__)
import JavaScript.Node.Buffer (toString__)
import JavaScript.Node.Crypto (randomBytes)
import Literals (StringLit, stringLit)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Untagged.Union (asOneOf)

newtype ByteCount = ByteCount Int

derive instance Newtype ByteCount _

generateHexString :: ∀ errors. ByteCount -> Async (InternalTerror_ errors) String
generateHexString (ByteCount byteCount) = do
    bytes <- randomBytes byteCount
        # fromEitherCont
        # lmap \error -> Terror internal__
            [ "There has been an error generating a hex string: " <> print error ]
    string <- toString__ (asOneOf (stringLit :: StringLit "hex")) bytes # fromEffect
    pure string
