module TeamTavern.Server.Infrastructure.GenerateHexString where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor.Label (label, relabelMap)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Node.Buffer (toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (print)
import Type.Proxy (Proxy(..))

newtype ByteCount = ByteCount Int

derive instance newtypeByteCount :: Newtype ByteCount _

type GenerateHexStringError errors = Variant (randomError :: Error | errors)

generateHexString :: forall errors. ByteCount -> Async (GenerateHexStringError errors) String
generateHexString (ByteCount byteCount) =
    label (Proxy :: _ "randomError") do
    bytes <- randomBytes byteCount # fromEitherCont
    string <- toString__ Hex bytes # fromEffect
    pure string

generateHexString' :: forall errors. ByteCount -> Async (InternalError errors) String
generateHexString'
    =   generateHexString
    >>> relabelMap (Proxy :: _ "randomError") (Proxy :: _ "internal") \error ->
        [ "There has been an error generating a hex string: " <> print error ]
