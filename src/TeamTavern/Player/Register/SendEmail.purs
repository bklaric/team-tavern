module TeamTavern.Player.Register.SendEmail
    (SendEmailError, sendRegistrationEmail) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Postmark.Async.Client (sendEmail)
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import TeamTavern.Player.Domain.Types (NoncedIdentifiers, Identifiers)

message :: NoncedIdentifiers -> Message
message { email, nickname, nonce } =
    { to: unwrap email
    , from: "admin@teamtavern.net"
    , subject: toNullable $ Just "TeamTavern registration"
    , textBody: toNullable $ Just $
        "Hi " <> unwrap nickname <> ",\n\n"
        <> "Your sign in code is " <> unwrap nonce <> ". "
        <> "Thank you for registering to TeamTavern."
    }

type SendEmailError errors = Variant
    ( sendEmailError ::
        { identifiers :: Identifiers
        , message :: Message
        , error :: Error
        }
    | errors )

sendRegistrationEmail :: forall errors.
    Maybe Client -> NoncedIdentifiers -> Async (SendEmailError errors) Unit
sendRegistrationEmail client identifiers @ { email, nickname } = let
    message' = message identifiers
    in
    case client of
        Nothing -> logShow message'
        Just client' ->
            client'
            # sendEmail message'
            # labelMap (SProxy :: SProxy "sendEmailError")
            { identifiers: { email, nickname }, message: message', error: _ }
