-- | Stands in for Discord's user endpoint in the test stack, so the suite can
-- | sign up and sign in with Discord without a Discord account. The access token
-- | is the user itself: the URI-encoded JSON of the record Discord would return.
module TeamTavern.DiscordStub.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (stripPrefix)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Foreign (unsafeToForeign)
import JSURI (decodeURIComponent)
import JavaScript.Node.Http.IncomingMessage (IncomingMessage, header, method, url)
import JavaScript.Node.Http.Server (createServer_C')
import JavaScript.Node.Http.ServerResponse (ServerResponse, writeHead_)
import JavaScript.Node.Net.Server (listen_)
import JavaScript.Node.Stream.Writable (end__, endString__)
import Yoga.JSON (readJSON_, writeJSON)

type DiscordUser =
    { id :: String
    , username :: String
    , discriminator :: String
    , email :: Maybe String
    , verified :: Maybe Boolean
    }

readUser :: IncomingMessage -> Maybe DiscordUser
readUser request =
    header "authorization" request
    >>= stripPrefix (Pattern "Bearer ")
    >>= decodeURIComponent
    >>= readJSON_

respond :: IncomingMessage -> ServerResponse -> Effect Unit
respond request response =
    case method request, url request, readUser request of
    Just "GET", Just "/api/users/@me", Just user -> do
        response # writeHead_ 200 (unsafeToForeign { "content-type": "application/json" })
        response # endString__ (writeJSON user) # void
    Just "GET", Just "/api/users/@me", Nothing -> do
        response # writeHead_ 401 (unsafeToForeign {})
        response # end__ # void
    _, _, _ -> do
        response # writeHead_ 404 (unsafeToForeign {})
        response # end__ # void

main :: Effect Unit
main = createServer_C' respond >>= listen_ { port: 3000 } # void
