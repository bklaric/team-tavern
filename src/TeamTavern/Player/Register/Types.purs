module TeamTavern.Player.Register.Types where

import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Variant (Variant)
import Foreign (ForeignError)
import Node.Errors (Error)
import Postgres.Error as Postgres
import Postmark.Error as Postmark
import Postmark.Message (Message)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (NonceError)
import TeamTavern.Player.Domain.Token (TokenError)
import TeamTavern.Player.Domain.Types (Identifiers)
import TeamTavern.Player.Infrastructure.ReadIdentifiers (IdentifiersError)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

type RegisterError = Variant
    ( signedIn ::
        { playerId :: String
        , cookies :: Map String String
        }
    , unreadableIdentifiers ::
        { content :: String
        , errors :: NonEmptyList ForeignError
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
    , emailTaken ::
        { email :: Email
        , error :: Postgres.Error
        }
    , nicknameTaken ::
        { nickname :: Nickname
        , error :: Postgres.Error
        }
    , databaseError :: Postgres.Error
    , sendEmailError ::
        { identifiers :: Identifiers
        , message :: Message
        , error :: Postmark.Error
        }
    )
