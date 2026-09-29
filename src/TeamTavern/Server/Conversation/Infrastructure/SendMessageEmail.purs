module TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (MessageEmail, sendMessageEmail) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe, maybe)
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, sendEmail)

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

messageEmail :: MessageEmail -> Email
messageEmail email =
    { to: email.to
    , subject: email.sender <> " sent you a message on TeamTavern"
    , blocks:
        [ Paragraph $ "Hi " <> email.recipient <> ","
        , Paragraph $ email.sender <> " wrote to you about " <> email.about <> ":"
        , Quote email.content
        , Button { label: "Reply", path: "/messages/" <> show email.conversation }
        ]
        <> maybe []
            (\nonce ->
                [ Paragraph "Your post has expired, so it's listed under older posts. Renew it to bring it back up:"
                , Button { label: "Renew", path: "/renew?nonce=" <> nonce }
                ])
            email.renewal
    , unsubscribe: true
    }

-- | A failed send is logged and doesn't fail the message, which is sent
-- | either way and waits in the inbox.
sendMessageEmail :: ∀ left. Mailer -> MessageEmail -> Async left Unit
sendMessageEmail mailer email =
    sendEmail mailer "Error sending message email" $ messageEmail email
