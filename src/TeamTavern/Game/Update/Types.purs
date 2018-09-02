module TeamTavern.Game.Update.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Description (DescriptionError)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Game.Domain.Name (Name, NameError)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)

type DetailsError = Variant
    ( name :: NonEmptyList NameError
    , handle :: NonEmptyList HandleError
    , description :: NonEmptyList DescriptionError
    )

type UpdateError = Variant
    ( invalidTargetHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    , cookiesNotPresent :: Map String String
    , unreadableDetailsModel ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidDetails ::
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
    , notAuthorized ::
        { id :: PlayerId
        , token :: Token
        , handle :: Handle
        }
    )
