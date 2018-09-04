module TeamTavern.Profile.ViewByGame.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (Foreign, ForeignError)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (HandleError)

type ViewByGameError = Variant
    ( invalidHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , databaseError :: Error
    , unreadableResult ::
        { rows :: Array Foreign
        , errors :: NonEmptyList ForeignError
        }
    , invalidViews :: NonEmptyList
        { nickname :: String
        , summary :: String
        }
    )
