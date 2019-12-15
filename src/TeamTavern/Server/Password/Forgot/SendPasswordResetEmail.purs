module TeamTavern.Server.Password.Forgot.SendPasswordResetEmail
    (SendEmailError, sendPasswordResetEmail) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (notNull, null)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Postmark.Async.Client (sendEmail)
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import TeamTavern.Server.Password.Forgot.AddPasswordReset (Player)
import TeamTavern.Server.Player.Domain.Nonce (Nonce)

type SendEmailError errors = Variant
    ( sendEmailError :: Error
    | errors )

message :: Player -> Nonce -> Message
message { email, nickname } nonce =
    { to: email
    , from: "TeamTavern admin@teamtavern.net"
    , subject: notNull "Password reset"
    , htmlBody: notNull $
        "Hi " <> nickname <> ",<br /><br />"
        <> "Open the link below to reset your TeamTavern account password:<br /><br />"
        <> "<a href=\"https://www.teamtavern.net/reset-password?nonce=" <> unwrap nonce <> "\">https://www.teamtavern.net/reset-password?nonce=" <> unwrap nonce <> "</a><br /><br />"
        <> "If you haven't made a password reset request, please ignore this email."
    , textBody: null
    }

sendPasswordResetEmail :: forall errors.
    Maybe Client -> Player -> Nonce -> Async (SendEmailError errors) Unit
sendPasswordResetEmail client player nonce =
    case client of
    Nothing -> logShow $ message player nonce
    Just client' ->
        client'
        # sendEmail (message player nonce)
        # label (SProxy :: SProxy "sendEmailError")
