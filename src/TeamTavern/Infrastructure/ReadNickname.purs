module TeamTavern.Infrastructure.ReadNickname where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Nickname as Nickname

type ReadNicknameError errors = Variant
    ( invalidNickname ::
        { nickname :: String
        , errors :: NonEmptyList NicknameError
        }
    | errors )

readNickname :: forall errors.
    String -> Async (ReadNicknameError errors) Nickname
readNickname nickname =
    nickname
    # Nickname.create'
    # labelMap (SProxy :: SProxy "invalidNickname") { nickname, errors: _ }
    # Async.fromEither
