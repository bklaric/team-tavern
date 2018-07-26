module TeamTavern.Player.Register.GenerateToken where

import Prelude

import Async (Async, fromEitherCont, fromEitherEffect)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Node.Buffer (Buffer, toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Domain.CharCount (CharCount, toByteCount)
import TeamTavern.Player.Domain.Nonce (Nonce, NonceError, nonceCharCount)
import TeamTavern.Player.Domain.Nonce as Nonce
import TeamTavern.Player.Domain.Token (Token, TokenError, tokenCharCount)
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Register.Types.Identifiers (Identifiers)

type GenerateTokenError = Variant
    ( token :: { errors :: NonEmptyList TokenError, identifiers :: Identifiers }
    , nonce :: { errors :: NonEmptyList NonceError, identifiers :: Identifiers }
    , random :: { error :: Error, identifiers :: Identifiers }
    )

generateRandomBytes :: Identifiers -> CharCount -> Async GenerateTokenError Buffer
generateRandomBytes identifiers charCount =
    charCount
    # toByteCount
    # unwrap
    # randomBytes
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
    token <- generateRandomBytes identifiers tokenCharCount
        >>= bufferToToken identifiers
    nonce <- generateRandomBytes identifiers nonceCharCount
        >>= bufferToNonce identifiers
    pure { token, nonce }
