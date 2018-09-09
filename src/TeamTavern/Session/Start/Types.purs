module TeamTavern.Session.Start.Types where

import Data.List.Types (NonEmptyList)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Postgres.Error (Error)
import Postgres.Result (Result)
import TeamTavern.Player.Domain.Types (NicknamedNonce)
import TeamTavern.Session.Infrastructure.Types (NicknamedNonceModel, IdentifiedTokenModel)
import TeamTavern.Session.Start.ReadNicknamedNonce (NicknamedNonceError)

type StartError = Variant
    ( unreadableNicknamedNonce ::
        { content :: String
        , errors :: NonEmptyList ForeignError
        }
    , invalidNicknamedNonce ::
        { nicknamedNonce :: NicknamedNonceModel
        , errors :: NonEmptyList NicknamedNonceError
        }
    , noTokenToConsume :: NicknamedNonce
    , databaseError :: Error
    , unreadableIdentifiedToken ::
        { result :: Result
        , errors :: NonEmptyList ForeignError
        }
    , invalidIdentifiedToken :: IdentifiedTokenModel
    )
