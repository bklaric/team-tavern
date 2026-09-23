module TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (MessageEmail, sendMessageEmail) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Effect.Class.Console (logShow)
import TeamTavern.Server.Infrastructure.Deployment (Deployment(..))
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Sendgrid (Message, sendAsync)

-- | An email about a message: `about` names the post as the recipient knows
-- | it, and `renewal` is the post's renewal nonce where it has expired and the
-- | recipient owns it.
type MessageEmail =
    { to :: String
    , recipient :: String
    , sender :: String
    , about :: String
    , conversation :: Int
    , content :: Array String
    , renewal :: Maybe String
    }

escape :: String -> String
escape = replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")

message :: Deployment -> MessageEmail -> Message
message deployment email = let
    origin = case deployment of
        -- Only logged, and the development and test stacks serve on different ports.
        Local -> ""
        Cloud -> "https://www.teamtavern.net"
    conversation = origin <> "/messages/" <> show email.conversation
    renewal = email.renewal <#> \nonce -> origin <> "/renew?nonce=" <> nonce
    link url = "<a href=\"" <> url <> "\">" <> url <> "</a>"
    in
    { to: email.to
    , from: "admin@teamtavern.net"
    , subject: email.sender <> " sent you a message on TeamTavern"
    , html: "Hi " <> escape email.recipient <> ",<br /><br />"
        <> escape email.sender <> " wrote to you about " <> escape email.about <> ":<br /><br />"
        <> "<blockquote>" <> joinWith "<br />" (escape <$> email.content) <> "</blockquote>"
        <> "Reply in your inbox:<br />" <> link conversation <> "<br /><br />"
        <> maybe "" (\url -> "Your post has expired, so it isn't in the feed. Renew it:<br />" <> link url) renewal
    , text: "Hi " <> email.recipient <> ",\n"
        <> email.sender <> " wrote to you about " <> email.about <> ":\n\n"
        <> joinWith "\n" email.content <> "\n\n"
        <> "Reply in your inbox:\n" <> conversation <> "\n"
        <> maybe "" (\url -> "\nYour post has expired, so it isn't in the feed. Renew it:\n" <> url <> "\n") renewal
    }

-- | A failed send is logged and doesn't fail the message, which is sent
-- | either way and waits in the inbox.
sendMessageEmail :: ∀ left. Deployment -> MessageEmail -> Async left Unit
sendMessageEmail deployment email = do
    result <- attempt case deployment of
        Local -> logShow $ message deployment email
        Cloud -> sendAsync $ message deployment email
    case result of
        Left error -> fromEffect $ logError "Error sending message email" error
        Right _ -> pure unit
