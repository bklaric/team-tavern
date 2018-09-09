module TeamTavern.Game.Update.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Description (DescriptionError)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Game.Domain.Name (Name, NameError)
import TeamTavern.Game.Infrastructure.Types (DetailsModel)
import TeamTavern.Player.Domain.Types (AuthInfo)

type DetailsError = Variant
    ( name :: NonEmptyList NameError
    , handle :: NonEmptyList HandleError
    , description :: NonEmptyList DescriptionError
    )

type UpdateError = Variant
    ( invalidHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , authNotPresent :: Map String String
    , unreadableDetails ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidDetails ::
        { details :: DetailsModel
        , errors :: NonEmptyList DetailsError
        }
    , nameTaken ::
        { name :: Name
        , error :: Error
        }
    , handleTaken ::
        { handle :: Handle
        , error :: Error
        }
    , databaseError :: Error
    , notAuthorized :: AuthInfo
    )
