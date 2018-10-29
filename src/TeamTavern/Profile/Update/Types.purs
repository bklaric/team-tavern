module TeamTavern.Profile.Update.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (HandleError)
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Player.Domain.Types (AuthInfo)
import TeamTavern.Profile.Domain.Summary (SummaryError)
import TeamTavern.Profile.Domain.Types (Identifiers)
import TeamTavern.Profile.Infrastructure.Types (IdentifiersModel)

type UpdateError = Variant
    ( invalidIdentifiers ::
        { identifiers :: IdentifiersModel
        , errors :: NonEmptyList (Variant
            ( invalidNickname :: NonEmptyList NicknameError
            , invalidHandle :: NonEmptyList HandleError
            ))
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
        , identifiers :: Identifiers
        }
    )
