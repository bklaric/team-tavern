module TeamTavern.Game.View.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)

type ViewError = Variant
    ( handleInvalid ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , databaseError :: Error
    , unreadableResult :: NonEmptyList ForeignError
    , notFound :: Handle
    , invalidView ::
        { administratorId :: Int
        , name :: String
        , handle :: Handle
        , description :: String
        }
    )
