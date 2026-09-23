module TeamTavern.Server.Infrastructure.Email
    (Block(..), Email, Mailer(..), deliver, sendEmail) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Either (Either(..))
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import TeamTavern.Server.Infrastructure.Log (logError, logStamped)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.Sendgrid (Message, sendAsync)

-- | Where the links in an email point, and whether it is sent or only logged.
-- | The origin is empty where emails are only read on the site's own origin:
-- | logged, or captured by the test stack's mail stub.
newtype Mailer = Mailer { origin :: String, send :: Boolean }

-- | An email is its blocks, which make both the HTML and the text body, so the
-- | two say the same thing. A button's path is on the site. A player's email
-- | sets `unsubscribe`, which links the account page's switches (brief 11.5).
type Email =
    { to :: String
    , subject :: String
    , blocks :: Array Block
    , unsubscribe :: Boolean
    }

data Block
    = Paragraph String
    | Quote (Array String)
    | Button { label :: String, path :: String }
    | Note String

escape :: String -> String
escape = replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")

-- The palette of tokens.css, inline, since email clients drop stylesheets.
font :: String
font = "font-family:Inter,-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;"

text :: String
text = "#EFE9E2"

muted :: String
muted = "#AFA397"

unsubscribePath :: String
unsubscribePath = "/account#emails"

blockHtml :: String -> Block -> String
blockHtml origin = case _ of
    Paragraph content ->
        "<p style=\"margin:0 0 16px;" <> font <> "font-size:16px;line-height:1.5;color:" <> text <> ";\">"
        <> escape content <> "</p>"
    Quote lines ->
        "<div style=\"margin:0 0 16px;padding:12px 16px;background:#2A241F;border-left:3px solid #F2823F;"
        <> "border-radius:4px;" <> font <> "font-size:16px;line-height:1.5;color:" <> text <> ";\">"
        <> joinWith "<br>" (escape <$> lines) <> "</div>"
    Button { label, path } ->
        "<p style=\"margin:0 0 16px;\"><a href=\"" <> escape (origin <> path) <> "\" style=\"display:inline-block;"
        <> "padding:10px 20px;background:#F2823F;border-radius:6px;" <> font
        <> "font-size:16px;font-weight:600;line-height:1.5;color:#1A0F0A;text-decoration:none;\">"
        <> escape label <> "</a></p>"
    Note content ->
        "<p style=\"margin:0 0 16px;" <> font <> "font-size:14px;line-height:1.5;color:" <> muted <> ";\">"
        <> escape content <> "</p>"

html :: String -> Email -> String
html origin email =
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
    <> "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    <> "<meta name=\"color-scheme\" content=\"dark\">"
    <> "<title>" <> escape email.subject <> "</title></head>"
    <> "<body style=\"margin:0;padding:0;background:#141210;\">"
    <> "<table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" style=\"background:#141210;\">"
    <> "<tr><td align=\"center\" style=\"padding:32px 16px;\">"
    <> "<table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" style=\"max-width:560px;\">"
    <> "<tr><td style=\"padding:0 0 16px;" <> font <> "font-size:20px;font-weight:700;color:" <> text <> ";\">"
    <> "TeamTavern</td></tr>"
    <> "<tr><td style=\"padding:24px 24px 8px;background:#1F1A16;border:1px solid #3B332C;border-radius:8px;\">"
    <> joinWith "" (blockHtml origin <$> email.blocks)
    <> "</td></tr>"
    <> (if email.unsubscribe
        then "<tr><td style=\"padding:16px 0 0;" <> font <> "font-size:14px;color:" <> muted <> ";\">"
            <> "<a href=\"" <> escape (origin <> unsubscribePath) <> "\" style=\"color:" <> muted <> ";\">"
            <> "Choose which emails you get</a></td></tr>"
        else "")
    <> "</table></td></tr></table></body></html>"

blockText :: String -> Block -> String
blockText origin = case _ of
    Paragraph content -> content
    Quote lines -> joinWith "\n" $ ("> " <> _) <$> lines
    Button { label, path } -> label <> ": " <> origin <> path
    Note content -> content

plain :: String -> Email -> String
plain origin email =
    joinWith "\n\n" (blockText origin <$> email.blocks)
    <> if email.unsubscribe
        then "\n\n--\nChoose which emails you get: " <> origin <> unsubscribePath <> "\n"
        else "\n"

message :: String -> Email -> Message
message origin email =
    { to: email.to
    , from: "admin@teamtavern.net"
    , subject: email.subject
    , html: html origin email
    , text: plain origin email
    }

-- | Sends the email, or logs its text where the mailer only logs.
deliver :: ∀ errors. Mailer -> Email -> Async (InternalTerror_ errors) Unit
deliver (Mailer { origin, send }) email
    | send = sendAsync $ message origin email
    | otherwise = fromEffect $ logStamped $
        "Email to " <> email.to <> " | " <> email.subject <> "\n" <> plain origin email

-- | Delivers the email and logs a failure under the heading rather than
-- | failing the request that sent it.
sendEmail :: ∀ left. Mailer -> String -> Email -> Async left Unit
sendEmail mailer heading email = do
    result <- attempt $ deliver mailer email
    case result of
        Left error -> fromEffect $ logError heading error
        Right _ -> pure unit
