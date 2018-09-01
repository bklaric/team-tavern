module TeamTavern.Player.Infrastructure.GenerateSecrets
    ( GenerateSecretsError
    , _generateSecrets
    , generateSecrets'
    , generateSecrets
    ) where

import Prelude

import Async (Async, fromEitherCont, fromEitherEffect)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant)
import Node.Buffer (Buffer, toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)
import Data.Bifunctor.Label (label)
import TeamTavern.Player.Domain.CharCount (CharCount, toByteCount)
import TeamTavern.Player.Domain.Nonce (Nonce, NonceError, nonceCharCount)
import TeamTavern.Player.Domain.Nonce as Nonce
import TeamTavern.Player.Domain.Token (Token, TokenError, tokenCharCount)
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Domain.Types (Secrets)

type GenerateSecretsError = Variant
    ( token :: NonEmptyList TokenError
    , nonce :: NonEmptyList NonceError
    , random :: Error
    )

generateRandomBytes :: CharCount -> Async GenerateSecretsError Buffer
generateRandomBytes charCount =
    charCount
    # toByteCount
    # unwrap
    # randomBytes
    # fromEitherCont
    # label (SProxy :: SProxy "random")

bufferToToken :: Buffer -> Async GenerateSecretsError Token
bufferToToken buffer =
    buffer
    # toString__ Hex
    <#> Token.create
    # fromEitherEffect
    # label (SProxy :: SProxy "token")

bufferToNonce :: Buffer -> Async GenerateSecretsError Nonce
bufferToNonce buffer =
    buffer
    # toString__ Hex
    <#> Nonce.create
    # fromEitherEffect
    # label (SProxy :: SProxy "nonce")

generateSecrets' :: Async GenerateSecretsError Secrets
generateSecrets' = do
    token <- generateRandomBytes tokenCharCount >>= bufferToToken
    nonce <- generateRandomBytes nonceCharCount >>= bufferToNonce
    pure { token, nonce }

_generateSecrets = SProxy :: SProxy "generateSecrets"

generateSecrets :: forall errors.
    Async (Variant (generateSecrets :: GenerateSecretsError | errors)) Secrets
generateSecrets = label _generateSecrets generateSecrets'
