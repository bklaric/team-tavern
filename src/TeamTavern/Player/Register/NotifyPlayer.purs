module TeamTavern.Player.Register.NotifyPlayer where

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
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postmark.Client (sendEmail)
import TeamTavern.Player.Register.Types.Credentials (IdentifiedCredentials)

message :: IdentifiedCredentials -> Message
message { id, email, nickname, token } =
        { to: unwrap email
        , from: "branimir.klaric1@xnet.hr"
        , subject: toNullable $ Just "TeamTavern registration"
        , textBody: toNullable $ Just $
            "Hi " <> unwrap nickname <> ",\n\n"
            <> "Your registration token is " <> unwrap token <> "."
        }

type SendEmailError = { error :: Error, credentials :: IdentifiedCredentials }

sendRegistrationEmail
    :: forall errors
    .  Client
    -> IdentifiedCredentials
    -> Async (Variant (sendEmail :: SendEmailError | errors)) Unit
sendRegistrationEmail client credentials =
    client
    # sendEmail (message credentials)
    # lmap { error: _, credentials}
    # label (SProxy :: SProxy "sendEmail")
    # void
