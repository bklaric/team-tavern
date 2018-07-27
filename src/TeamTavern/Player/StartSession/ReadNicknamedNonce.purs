module TeamTavern.Player.StartSession.ReadNicknamedNonce
    ( ReadNicknameError
    , ReadNonceError
    , readNicknamedNonce
    ) where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async as Async
import TeamTavern.Architecture.Either (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.Nonce (Nonce, NonceError)
import TeamTavern.Player.Domain.Nonce as Nonce
import Validated (toEither)

type ReadNicknameError =
    { errors :: NonEmptyList NicknameError
    , nickname :: NonEmptyString
    }

_readNickname = SProxy :: SProxy "readNickname"

readNickname
    :: forall errors
    .  NonEmptyString
    -> Async (Variant (readNickname :: ReadNicknameError | errors)) Nickname
readNickname nickname =
    nickname
    # toString
    # Nickname.create
    # toEither
    # lmap { errors: _, nickname}
    # label _readNickname
    # fromEither

type ReadNonceError = Variant
    ( invalidBody ::
        { errors :: MultipleErrors, body :: String }
    , invalidNonce ::
        { errors :: NonEmptyList NonceError, nonce :: String }
    )

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
