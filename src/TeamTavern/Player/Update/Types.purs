module TeamTavern.Player.Update.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.Domain.Types (AuthInfo)
import TeamTavern.Player.Infrastructure.Types (NicknamedAboutModel)
import TeamTavern.Player.Update.ReadUpdate (NicknamedAboutError)

type UpdateError = Variant
    ( invalidNickname ::
        { nickname :: String
        , errors :: NonEmptyList NicknameError
        }
    , authNotPresent :: Map String String
    , unreadableUpdate ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidUpdate ::
        { nicknamedAbout :: NicknamedAboutModel
        , errors :: NonEmptyList NicknamedAboutError
        }
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized ::
        { authInfo :: AuthInfo
        , nickname :: Nickname
        }
    )
