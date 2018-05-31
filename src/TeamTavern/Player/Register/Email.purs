module TeamTavern.Player.Register.Email where

import Prelude

import Async (Async)
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
import TeamTavern.Player.Credentials (Credentials)

message :: Credentials -> Message
message { email, nickname, token } =
        { to: unwrap email
        , from: "branimir.klaric1@xnet.hr"
        , subject: toNullable $ Just "TeamTavern registration"
        , textBody: toNullable $ Just $
            "Hi " <> unwrap nickname <> ",\n\n"
            <> "Your registration token is " <> unwrap token <> "."
        }

sendRegistrationEmail :: forall errors.
    Client -> Credentials -> Async (Variant (email :: Error | errors)) Unit
sendRegistrationEmail client playerToRegister =
    client
    # sendEmail (message playerToRegister)
    # label (SProxy :: SProxy "email")
    # void
