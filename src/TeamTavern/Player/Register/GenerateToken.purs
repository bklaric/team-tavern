module TeamTavern.Player.Register.GenerateToken where

import Prelude

import Async (Async, fromEitherCont, fromEitherEffect)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Node.Buffer (Buffer, toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Identifiers (Identifiers)
import TeamTavern.Player.Token (Token, TokenError, tokenCharCount)
import TeamTavern.Player.Token as Token

type GenerateTokenError = Variant
    ( token :: { errors :: NonEmptyList TokenError, identifiers :: Identifiers }
    , random :: { error :: Error, identifiers :: Identifiers }
    )

tokenByteCount :: Int
tokenByteCount = tokenCharCount / 2

generateRandomBytes :: Identifiers -> Async GenerateTokenError Buffer
generateRandomBytes identifiers =
    randomBytes tokenByteCount
    # fromEitherCont
    # lmap { error: _, identifiers }
    # label (SProxy :: SProxy "random")

bufferToToken :: Identifiers -> Buffer -> Async GenerateTokenError Token
bufferToToken identifiers buffer =
    buffer
    # toString__ Hex
    <#> Token.create
    # fromEitherEffect
    # lmap { errors: _, identifiers}
    # label (SProxy :: SProxy "token")

generateToken
    :: forall errors
    .  Identifiers
    -> Async (Variant (generateToken :: GenerateTokenError | errors)) Token
generateToken identifiers =
    generateRandomBytes identifiers
    >>= bufferToToken identifiers
    # label (SProxy :: SProxy "generateToken")
