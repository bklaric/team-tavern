module TeamTavern.Profile.Update.Types where


import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import TeamTavern.Game.Domain.Handle (HandleError)
import TeamTavern.Player.Domain.Nickname (NicknameError)
import TeamTavern.Player.Domain.Types (IdentifiedToken')
import TeamTavern.Profile.Domain.Summary (SummaryError)
import TeamTavern.Profile.Domain.Types (Identifiers)

type UpdateError = Variant
    ( invalidIdentifiers ::
        { nickname :: String
        , handle :: String
        , errors :: NonEmptyList (Variant
            ( invalidNickname :: NonEmptyList NicknameError
            , invalidHandle :: NonEmptyList HandleError
            ))
        }
    , authNotPresent :: Map String String
    , unreadableSummary ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidSummary ::
        { summary :: String
        , errors :: NonEmptyList SummaryError
        }
    , databaseError :: Error
    , notAuthorized ::
        { auth :: IdentifiedToken'
        , identifiers :: Identifiers
        }
    )
