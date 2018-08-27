module TeamTavern.Player.Update.Types where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Player.Domain.About (AboutError)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)

-- Error for the Update type.
type UpdateError' = Variant
    ( nickname :: NonEmptyList NicknameError
    , about :: NonEmptyList AboutError
    )

-- Error for the update use case.
type UpdateError = Variant
    ( cantValidateTargetNickname :: NonEmptyList NicknameError
    , cookiesNotPresent :: Unit
    , nicknamesNotSame :: Unit
    , cantReadUpdateModel :: NonEmptyList ForeignError
    , cantValidateUpdate :: NonEmptyList UpdateError'
    , nicknameTaken :: { nickname :: Nickname, error :: Error }
    , databaseError :: Error
    , notAuthorized :: Unit
    )
