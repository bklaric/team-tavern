module TeamTavern.Player.Register.NotifyPlayer
    (SendEmailError, sendRegistrationEmail) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import Data.Bifunctor.Label (label)
import TeamTavern.Architecture.Postmark.Client (sendEmail)
import TeamTavern.Player.Domain.Types (NoncedIdentifiers)

message :: NoncedIdentifiers -> Message
message { email, nickname, nonce } =
    { to: unwrap email
    , from: "branimir.klaric1@xnet.hr"
    , subject: toNullable $ Just "TeamTavern registration"
    , textBody: toNullable $ Just $
        "Hi " <> unwrap nickname <> ",\n\n"
        <> "Your sign in code is " <> unwrap nonce <> ". "
        <> "Thank you for registering to TeamTavern."
    }

type SendEmailError =
    { error :: Error
    , identifiers :: NoncedIdentifiers
    }

sendRegistrationEmail
    :: forall errors
    .  Client
    -> NoncedIdentifiers
    -> Async (Variant (sendEmail :: SendEmailError | errors)) Unit
sendRegistrationEmail client identifiers =
    client
    # sendEmail (message identifiers)
    # lmap { error: _, identifiers}
    # label (SProxy :: SProxy "sendEmail")
