module TeamTavern.Player.Infrastructure where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Bifunctor.Label (label)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname
import Validated (toEither)

type ReadNicknameError =
    { errors :: NonEmptyList NicknameError
    , nickname :: NonEmptyString
    }

readNickname' :: NonEmptyString -> Async ReadNicknameError Nickname
readNickname' nickname =
    nickname
    # toString
    # Nickname.create
    # toEither
    # lmap { errors: _, nickname}
    # fromEither

readNickname
    :: forall errors
    .  NonEmptyString
    -> Async (Variant (readNickname :: ReadNicknameError | errors)) Nickname
readNickname = readNickname' >>> label (SProxy :: SProxy "readNickname")
