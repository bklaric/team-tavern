module TeamTavern.Player.Register.SendEmail
    (SendEmailModel, SendEmailError, sendEmail) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Postmark.Async.Client as Postmark
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import TeamTavern.Player.Register.GenerateNonce (Nonce, unNonce)
import TeamTavern.Player.Register.ValidateModel (Email, Nickname, unEmail, unNickname)

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
    { to: unEmail email
    , from: "admin@teamtavern.net"
    , subject: toNullable $ Just "TeamTavern registration"
    , htmlBody: toNullable $ Just $
        "Hi " <> unNickname nickname <> ",<br /><br />"
        <> "Please <a href=\"https://www.teamtavern.net/signin?nonce=" <> unNonce nonce <> "\">click here</a> to verify your email. "
        <> "Thank you for registering to TeamTavern."
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
