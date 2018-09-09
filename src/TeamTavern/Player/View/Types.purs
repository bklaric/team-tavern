module TeamTavern.Player.View.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import Postgres.Result (Result)
import TeamTavern.Player.Domain.Nickname (Nickname, NicknameError)
import TeamTavern.Player.View.LoadPlayer (PlayerViewModel)

type ViewError = Variant
    ( invalidNickname ::
        { nickname :: String
        , errors :: NonEmptyList NicknameError
        }
    , databaseError :: Error
    , unreadableResult ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , notFound :: Nickname
    , invalidView ::
        { nickname :: Nickname
        , view :: PlayerViewModel
        }
    )
