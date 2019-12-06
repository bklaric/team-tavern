module TeamTavern.Server.Conversation.Start.NotifyReceiver
    (Nickname, Email, NotifyReceiverModel, NotifyReceiverError, notifyReceiver) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (label)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Postmark.Async.Client as Postmark
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)

type Nickname = String

type Email = String

type NotifyReceiverModel =
    { sender :: Nickname
    , receiver :: Nickname
    , receiverEmail :: Email
    }

type NotifyReceiverError errors = Variant
    ( sendEmailError :: Error
    | errors )

message :: NotifyReceiverModel -> Message
message { sender, receiver, receiverEmail } =
    { to: receiverEmail
    , from: "TeamTavern admin@teamtavern.net"
    , subject: toNullable $ Just $ "Message from " <> sender
    , htmlBody: toNullable $ Just $
        "Hi " <> receiver <> ",<br /><br />"
        <> "You have received a message from " <> sender <>". "
        <> "<a href=\"https://www.teamtavern.net/signin\">Sign in to TeamTavern</a> "
        <> "and open your account conversations to view the message.<br /><br />"
        <> "<span style=\"font-size: 12px; color: #888;\">To stop receiving these notifications edit your TeamTavern account preferences.</span>"
    , textBody: toNullable Nothing
    }

notifyReceiver :: forall errors.
    Maybe Client -> NotifyReceiverModel -> Async (NotifyReceiverError errors) Unit
notifyReceiver client model @ { receiver, receiverEmail } = let
    message' = message model
    in
    case client of
        Nothing -> logShow message'
        Just client' ->
            client'
            # Postmark.sendEmail message'
            # label (SProxy :: SProxy "sendEmailError")
