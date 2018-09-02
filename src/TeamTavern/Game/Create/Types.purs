module TeamTavern.Game.Create.Types where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Description (DescriptionError)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Game.Domain.Name (Name, NameError)

type DetailsError = Variant
    ( name :: NonEmptyList NameError
    , handle :: NonEmptyList HandleError
    , description :: NonEmptyList DescriptionError
    )

type CreateError = Variant
    ( cookiesNotPresent :: Map String String
    , cantReadDetailsModel ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , cantValidateDetails ::
        { name :: String
        , handle :: String
        , description :: String
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
    , notAuthorized :: Unit
    )
