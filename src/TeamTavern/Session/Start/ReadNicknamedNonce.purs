module TeamTavern.Session.Start.ReadNicknamedNonce where

import Prelude

import Async (Async)
import Async.Validated as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Nonce (NonceError)
import TeamTavern.Player.Domain.Nonce as Nonce
import TeamTavern.Player.Domain.Types (NicknamedNonce)
import TeamTavern.Session.Infrastructure.Types (NicknamedNonceModel)

type NicknamedNonceError = Variant
    ( nickname :: NonEmptyList NicknameError
    , nonce :: NonEmptyList NonceError
    )

type ReadNonceError errors = Variant
    ( unreadableNicknamedNonce ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidNicknamedNonce ::
        { nicknamedNonce :: NicknamedNonceModel
        , errors :: NonEmptyList NicknamedNonceError
        }
    | errors )

readNicknamedNonce :: forall errors.
    Body -> Async (ReadNonceError errors) NicknamedNonce
readNicknamedNonce body = do
    content <- readBody body
    nicknamedNonce @ { nickname, nonce } :: NicknamedNonceModel <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableNicknamedNonce")
            { content, errors: _ }
    { nickname: _, nonce: _ }
        <$> (Nickname.create nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (Nonce.create nonce
            # Validated.label (SProxy :: SProxy "nonce"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidNicknamedNonce")
            { nicknamedNonce, errors: _ }
