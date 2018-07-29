module TeamTavern.Player.Session.Prepare.ReadNickname where

import Prelude

import Async (Async)
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Architecture.Async as Async
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Infrastructure (ReadNicknameError)
import TeamTavern.Player.Infrastructure as Infrastructure

readNickname
    :: forall errors
    .  NonEmptyString
    -> Async (Variant (readNickname :: ReadNicknameError | errors)) Nickname
readNickname nickname =
    Infrastructure.readNickname nickname
    # Async.label (SProxy :: SProxy "readNickname")
