module TeamTavern.Profile.ViewByGame.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import Postgres.Result (Result)
import TeamTavern.Game.Domain.Handle (HandleError)

type ViewByGameError = Variant
    ( invalidHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , databaseError :: Error
    , unreadableViews ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , invalidViews :: NonEmptyList
        { nickname :: String
        , summary :: String
        }
    )
