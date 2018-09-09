module TeamTavern.Game.ViewAll.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import Postgres.Result (Result)
import TeamTavern.Game.ViewAll.LoadGames (GameViewModel)

type ViewAllError = Variant
    ( unreadableViews ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , invalidViews :: NonEmptyList GameViewModel
    , databaseError :: Error
    )
