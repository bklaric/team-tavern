module TeamTavern.Game.View.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import Postgres.Result (Result)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Game.View.LoadGame (GameViewModel)

type ViewError = Variant
    ( invalidHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , databaseError :: Error
    , unreadableView ::
        { result :: Result
        , errors :: MultipleErrors
        }
    , notFound :: Handle
    , invalidView ::
        { handle :: Handle
        , view :: GameViewModel
        }
    )
