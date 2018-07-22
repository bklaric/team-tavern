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
import TeamTavern.Player.Register.Types.Identifiers (Identifiers)
import TeamTavern.Player.Domain.Nonce (Nonce, NonceError, nonceCharCount)
import TeamTavern.Player.Domain.Nonce as Nonce
import TeamTavern.Player.Domain.Token (Token, TokenError, tokenCharCount)
import TeamTavern.Player.Domain.Token as Token

type GenerateTokenError = Variant
    ( token :: { errors :: NonEmptyList TokenError, identifiers :: Identifiers }
    , nonce :: { errors :: NonEmptyList NonceError, identifiers :: Identifiers }
    , random :: { error :: Error, identifiers :: Identifiers }
    )

tokenByteCount :: Int
tokenByteCount = tokenCharCount / 2

nonceByteCount :: Int
nonceByteCount = nonceCharCount / 2

generateRandomBytes :: Identifiers -> Int -> Async GenerateTokenError Buffer
generateRandomBytes identifiers byteCount =
    randomBytes byteCount
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

bufferToNonce :: Identifiers -> Buffer -> Async GenerateTokenError Nonce
bufferToNonce identifiers buffer =
    buffer
    # toString__ Hex
    <#> Nonce.create
    # fromEitherEffect
    # lmap { errors: _, identifiers}
    # label (SProxy :: SProxy "nonce")

generateToken
    :: forall errors
    .  Identifiers
    -> Async
        (Variant (generateToken :: GenerateTokenError | errors))
        { token :: Token, nonce :: Nonce }
generateToken identifiers = label (SProxy :: SProxy "generateToken") $ do
    token <- generateRandomBytes identifiers tokenByteCount
        >>= bufferToToken identifiers
    nonce <- generateRandomBytes identifiers nonceByteCount
        >>= bufferToNonce identifiers
    pure { token, nonce }
