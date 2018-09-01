module TeamTavern.Game.ViewAll.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)

type ViewAllError = Variant
    ( invalidViews :: NonEmptyList
        { administratorId :: Int
        , name :: String
        , handle :: String
        , description :: String
        }
    , databaseError :: Error
    , unreadableResult :: NonEmptyList ForeignError
    )
