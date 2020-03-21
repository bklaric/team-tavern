module TeamTavern.Server.Player.ChangeNickname.ReadNickname where

import Prelude

import Async (Async)
import Async (fromEither) as Async
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Server.Player.Domain.Nickname as Nickname

type ChangeNicknameDto = { nickname :: String }

type ChangeNicknameModel = { nickname :: Nickname }

type ReadNicknameError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidModel ::
        { dto :: ChangeNicknameDto
        , errors :: NonEmptyList NicknameError
        }
    | errors )

readNickname :: forall errors.
    Body -> Async (ReadNicknameError errors) ChangeNicknameModel
readNickname body = do
    content <- readBody body
    dto @ { nickname } :: ChangeNicknameDto <-
        readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
        # Async.fromEither
    { nickname: _ }
        <$> Nickname.create nickname
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel") { dto, errors: _ }
