module TeamTavern.Player.SignIn.ReadNickname where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Architecture.Either (label)
import TeamTavern.Player.Nickname (Nickname, NicknameError, create)
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
    # create
    # toEither
    # lmap { errors: _, nickname}
    # label _readNickname
    # fromEither
