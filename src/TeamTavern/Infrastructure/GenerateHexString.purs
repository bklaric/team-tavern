module TeamTavern.Infrastructure.GenerateHexString where

import Prelude

import Async (Async, fromEffect, fromEitherCont)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant)
import Node.Buffer (toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)

class HexString string where
    fromHexString :: String -> string

instance stringHexString :: HexString String where
    fromHexString = identity

type GenerateHexStringError errors = Variant (randomError :: Error | errors)

generateHexString :: forall result errors. HexString result =>
    Int -> Async (GenerateHexStringError errors) result
generateHexString characterCount =
    label (SProxy :: SProxy "randomError") do
    let byteCount = characterCount / 2 -- Two hex characters per byte.
    bytes <- randomBytes byteCount # fromEitherCont
    string <- toString__ Hex bytes # fromEffect
    pure $ fromHexString string
