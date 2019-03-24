module TeamTavern.Infrastructure.GenerateHexString where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor.Label (label)
import Data.Newtype (class Newtype)
import Data.Variant (SProxy(..), Variant)
import Node.Buffer (toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)

newtype ByteCount = ByteCount Int

derive instance newtypeByteCount :: Newtype ByteCount _

type GenerateHexStringError errors = Variant (randomError :: Error | errors)

generateHexString :: forall errors.
    ByteCount -> Async (GenerateHexStringError errors) String
generateHexString (ByteCount byteCount) =
    label (SProxy :: SProxy "randomError") do
    bytes <- randomBytes byteCount # fromEitherCont
    string <- toString__ Hex bytes # fromEffect
    pure string
