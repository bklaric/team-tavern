module TeamTavern.Profile.Create.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Player.Domain.Types (AuthInfo)
import TeamTavern.Profile.Domain.Summary (SummaryError)

type CreateError = Variant
    ( invalidHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , authNotPresent :: Map String String
    , unreadableSummary ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidSummary ::
        { summary :: String
        , errors :: NonEmptyList SummaryError
        }
    , databaseError :: Error
    , notAuthorized ::
        { auth :: AuthInfo
        , handle :: Handle
        }
    )
