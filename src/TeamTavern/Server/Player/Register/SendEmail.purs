module TeamTavern.Server.Player.Register.SendEmail
    (SendEmailModel, SendEmailError, sendEmail) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Postmark.Async.Client as Postmark
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import TeamTavern.Server.Player.Domain.Email (Email)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Domain.Nonce (Nonce)

type SendEmailModel =
    { email :: Email
    , nickname :: Nickname
    , nonce :: Nonce
    }

type SendEmailError errors = Variant
    ( sendEmailError ::
        { info :: { email :: Email, nickname :: Nickname }
        , error :: Error
        }
    | errors )

message :: SendEmailModel -> Message
message { email, nickname, nonce } =
    { to: unwrap email
    , from: "TeamTavern admin@teamtavern.net"
    , subject: toNullable $ Just "TeamTavern registration"
    , htmlBody: toNullable $ Just $
        "Hi " <> unwrap nickname <> ",<br /><br />"
        <> "Thank you for registering to TeamTavern. "
        <> "Please open the link below to sign in and verify your email address:<br /><br />"
        <> "<a href=\"https://www.teamtavern.net/signin?nonce=" <> unwrap nonce <> "\">https://www.teamtavern.net/signin?nonce=" <> unwrap nonce <> "</a><br /><br />"
        <> "Should you have any questions or feedback, please contact <a href=\"mailto:admin@teamtavern.net\">admin@teamtavern.net</a>. Thank you for your time.<br /><br />"
        <> "Happy playing!"
    , textBody: toNullable Nothing
    }

sendEmail :: forall errors.
    Maybe Client -> SendEmailModel -> Async (SendEmailError errors) Unit
sendEmail client model @ { email, nickname } = let
    message' = message model
    in
    case client of
        Nothing -> logShow message'
        Just client' ->
            client'
            # Postmark.sendEmail message'
            # labelMap (SProxy :: SProxy "sendEmailError")
                { info: { email, nickname }, error: _ }
