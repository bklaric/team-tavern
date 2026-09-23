-- | Stands in for SendGrid in the test stack. It keeps every email the server
-- | sends and shows those to an address at `/mail?to=<address>`, which Caddy
-- | serves on the site's own origin, so a spec opens its player's mail in the
-- | browser and follows the links in it as the player would.
module TeamTavern.MailStub.Main (main) where

import Prelude

import Data.Array (concatMap, filter, find, null)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, stripPrefix)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import JSURI (decodeURIComponent)
import JavaScript.Node.Buffer (concat_, toString___)
import JavaScript.Node.Http.IncomingMessage (IncomingMessage, method, url)
import JavaScript.Node.Http.Server (createServer_C')
import JavaScript.Node.Http.ServerResponse (ServerResponse, writeHead_)
import JavaScript.Node.Net.Server (listen_)
import JavaScript.Node.Stream.Readable.Events (collectDataEvents)
import JavaScript.Node.Stream.Writable (end__, endString__)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (readJSON_)

type Mail = { to :: String, subject :: String, html :: String }

-- | The part of SendGrid's mail send request the page shows.
type SendRequest =
    { personalizations :: Array { to :: Array { email :: String } }
    , subject :: String
    , content :: Array { type :: String, value :: String }
    }

mailsOf :: SendRequest -> Array Mail
mailsOf request = let
    html = request.content # find (_.type >>> eq "text/html") # maybe "" _.value
    in
    request.personalizations
    # concatMap _.to
    <#> \{ email } -> { to: email, subject: request.subject, html }

escape :: String -> String
escape = replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")

-- An email's links open in the tab, not in its frame.
mailHtml :: Mail -> String
mailHtml { subject, html } =
    "<article><h2>" <> escape subject <> "</h2>"
    <> "<iframe title=\"" <> escape subject <> "\" srcdoc=\""
    <> escape ("<base target=\"_top\">" <> html) <> "\"></iframe></article>"

page :: String -> Array Mail -> String
page to mails =
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
    <> "<title>Mail to " <> escape to <> "</title>"
    <> "<style>body { font-family: sans-serif; margin: 24px; } "
    <> "iframe { width: 100%; height: 560px; border: 1px solid #ccc; }</style></head>"
    <> "<body><h1>Mail to " <> escape to <> "</h1>"
    <> (if null mails then "<p>No emails.</p>" else joinWith "" (mailHtml <$> mails))
    <> "</body></html>"

reply :: Int -> String -> String -> ServerResponse -> Effect Unit
reply status contentType body response = do
    response # writeHead_ status (unsafeToForeign { "content-type": contentType })
    response # endString__ body # void

respond :: Ref (Array Mail) -> IncomingMessage -> ServerResponse -> Effect Unit
respond store request response =
    case method request, url request of
    Just "POST", Just "/v3/mail/send" ->
        request # collectDataEvents (map unsafeCoerce >>> concat_ >=> toString___ >=> \body ->
            case readJSON_ body of
            Just sendRequest -> do
                -- Newest first.
                Ref.modify_ (\mails -> mailsOf sendRequest <> mails) store
                response # writeHead_ 202 (unsafeToForeign {})
                response # end__ # void
            Nothing -> response # reply 400 "text/plain" "Not a mail send request")
        # void
    Just "GET", Just path | Just to <- stripPrefix (Pattern "/mail?to=") path >>= decodeURIComponent -> do
        mails <- Ref.read store <#> filter (_.to >>> eq to)
        response # reply 200 "text/html; charset=utf-8" (page to mails)
    _, _ -> do
        response # writeHead_ 404 (unsafeToForeign {})
        response # end__ # void

main :: Effect Unit
main = do
    store <- Ref.new []
    createServer_C' (respond store) >>= listen_ { port: 3000 } # void
