module TeamTavern.Player.Infrastructure.GenerateSecrets
    ( GenerateSecretsError
    , generateSecrets
    ) where

import Prelude

import Async (Async, fromEffect, fromEither, fromEitherCont)
import Data.Bifunctor.Label (label, labelMap)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant)
import Node.Buffer (Buffer, toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)
import TeamTavern.Player.Domain.CharCount (CharCount, toByteCount)
import TeamTavern.Player.Domain.Nonce (Nonce, NonceError, nonceCharCount)
import TeamTavern.Player.Domain.Nonce as Nonce
import TeamTavern.Player.Domain.Token (Token, TokenError, tokenCharCount)
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Domain.Types (Secrets)

type GenerateSecretsError errors = Variant
    ( invalidGeneratedToken ::
        { token :: String
        , errors :: NonEmptyList TokenError
        }
    , invalidGeneratedNonce ::
        { nonce :: String
        , errors :: NonEmptyList NonceError
        }
    , randomError :: Error
    | errors )

generateRandomBytes :: forall errors.
    CharCount -> Async (GenerateSecretsError errors) Buffer
generateRandomBytes charCount =
    charCount
    # toByteCount
    # unwrap
    # randomBytes
    # fromEitherCont
    # label (SProxy :: SProxy "randomError")

bufferToToken :: forall errors.
    Buffer -> Async (GenerateSecretsError errors) Token
bufferToToken buffer =  do
    token <- buffer # toString__ Hex # fromEffect
    token
        # Token.create'
        # fromEither
        # labelMap (SProxy :: SProxy "invalidGeneratedToken")
            { token, errors: _ }

bufferToNonce :: forall errors.
    Buffer -> Async (GenerateSecretsError errors) Nonce
bufferToNonce buffer = do
    nonce <- buffer # toString__ Hex # fromEffect
    nonce
        # Nonce.create'
        # fromEither
        # labelMap (SProxy :: SProxy "invalidGeneratedNonce")
            { nonce, errors: _ }

generateSecrets :: forall errors. Async (GenerateSecretsError errors) Secrets
generateSecrets = do
    token <- generateRandomBytes tokenCharCount >>= bufferToToken
    nonce <- generateRandomBytes nonceCharCount >>= bufferToNonce
    pure { token, nonce }
