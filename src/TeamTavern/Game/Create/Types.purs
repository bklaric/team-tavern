module TeamTavern.Game.Create.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (Handle)
import TeamTavern.Game.Domain.Title (Title)
import TeamTavern.Game.Infrastructure.ReadDetails (DetailsError)
import TeamTavern.Game.Infrastructure.Types (DetailsModel)
import TeamTavern.Player.Domain.Types (AuthInfo)

type CreateError = Variant
    ( authNotPresent :: Map String String
    , unreadableDetails ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidDetails ::
        { details :: DetailsModel
        , errors :: NonEmptyList DetailsError
        }
    , titleTaken ::
        { title :: Title
        , error :: Error
        }
    , handleTaken ::
        { handle :: Handle
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized :: AuthInfo
    )
