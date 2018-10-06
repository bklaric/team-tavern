module TeamTavern.Session.Prepare.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Node.Errors (Error)
import Postgres.Error as Postgres
import Postmark.Error as Postmark
import Postmark.Message (Message)
import TeamTavern.Player.Domain.Nonce (NonceError)
import TeamTavern.Player.Domain.Token (TokenError)
import TeamTavern.Player.Domain.Types (Identifiers)
import TeamTavern.Player.Infrastructure.ReadIdentifiers (IdentifiersError)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

type PrepareError = Variant
    ( signedIn ::
        { playerId :: String
        , cookies :: Map String String
        }
    , unreadableIdentifiers ::
        { content :: String
        , errors :: MultipleErrors
        }
    , invalidIdentifiers ::
        { identifiers :: IdentifiersModel
        , errors :: NonEmptyList IdentifiersError
        }
    , randomError :: Error
    , invalidGeneratedNonce ::
        { nonce :: String
        , errors :: NonEmptyList NonceError
        }
    , invalidGeneratedToken ::
        { token :: String
        , errors :: NonEmptyList TokenError
        }
    , databaseError :: Postgres.Error
    , unknownIdentifiers :: Identifiers
    , sendEmailError ::
        { message :: Message
        , error :: Postmark.Error
        }
    )
