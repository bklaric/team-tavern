module TeamTavern.Profile.View.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (Foreign, MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (HandleError)
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Profile.Domain.Types (Identifiers)
import TeamTavern.Profile.Infrastructure.Types (IdentifiersModel)

type ViewError = Variant
    ( invalidIdentifiers ::
        { identifiers :: IdentifiersModel
        , errors :: NonEmptyList (Variant
            ( invalidNickname :: NonEmptyList NicknameError
            , invalidHandle :: NonEmptyList HandleError
            ))
        }
    , databaseError :: Error
    , unreadableViewModel ::
        { foreignViewModel :: Foreign
        , errors :: MultipleErrors
        }
    , notFound :: Identifiers
    , invalidView :: { summary :: String }
    )
