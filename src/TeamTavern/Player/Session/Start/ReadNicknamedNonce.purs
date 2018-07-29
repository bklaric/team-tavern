module TeamTavern.Player.Session.Start.ReadNicknamedNonce
    (ReadNonceError, readNicknamedNonce) where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async as Async
import TeamTavern.Architecture.Either (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce, NonceError)
import TeamTavern.Player.Domain.Nonce as Nonce
import TeamTavern.Player.Infrastructure (ReadNicknameError)
import TeamTavern.Player.Infrastructure as Infrastructure

type ReadNonceError = Variant
    ( invalidBody ::
        { errors :: MultipleErrors, body :: String }
    , invalidNonce ::
        { errors :: NonEmptyList NonceError, nonce :: String }
    )

readNickname
    :: forall errors
    .  NonEmptyString
    -> Async (Variant (readNickname :: ReadNicknameError | errors)) Nickname
readNickname nickname =
    Infrastructure.readNickname nickname
    # Async.label (SProxy :: SProxy "readNickname")

readNonce
    :: forall errors
    .  Body
    -> Async (Variant (readNonce :: ReadNonceError | errors)) Nonce
readNonce body = Async.label (SProxy :: SProxy "readNonce") do
    content <- readBody body
    case readJSON content of
        Left errors ->
            { errors, body: content }
            # Left
            # label (SProxy :: SProxy "invalidBody")
            # fromEither
        Right ({ nonce } :: { nonce :: String }) ->
            Nonce.create nonce
            # lmap { errors: _, nonce }
            # label (SProxy :: SProxy "invalidNonce")
            # fromEither

readNicknamedNonce
    :: forall errors
    .  NonEmptyString
    -> Body
    -> Async (Variant
        ( readNickname :: ReadNicknameError
        , readNonce :: ReadNonceError
        | errors
        ))
        { nickname :: Nickname
        , nonce :: Nonce
        }
readNicknamedNonce nickname' body = do
    nickname <- readNickname nickname'
    nonce <- readNonce body
    pure { nickname, nonce }
