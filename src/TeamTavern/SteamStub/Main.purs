-- | Stands in for the Steam Web API's ResolveVanityURL in the test stack, so the
-- | suite can give a custom Steam profile address without reaching Steam. It
-- | knows one: `gabelogannewell`, the custom address of Gabe Newell's profile. Asked for
-- | `steam-is-down` it answers 503, as Steam does when it's down. Every other
-- | name has no profile, and a request without a key is refused, as Steam
-- | refuses it.
module TeamTavern.SteamStub.Main (main) where

import Prelude

import Data.Array (mapMaybe)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (unsafeToForeign)
import JSURI (decodeURIComponent)
import JavaScript.Node.Http.IncomingMessage (IncomingMessage, method, url)
import JavaScript.Node.Http.Server (createServer_C')
import JavaScript.Node.Http.ServerResponse (ServerResponse, writeHead_)
import JavaScript.Node.Net.Server (listen_)
import JavaScript.Node.Stream.Writable (end__, endString__)
import TeamTavern.Server.Infrastructure.ResolveSteamId (VanityContent)
import Yoga.JSON (writeJSON)

vanities :: Array (Tuple String String)
vanities = [ Tuple "gabelogannewell" "76561197960287930" ]

queryParams :: String -> Array (Tuple String String)
queryParams query = split (Pattern "&") query # mapMaybe \pair ->
    case split (Pattern "=") pair <#> decodeURIComponent of
        [ Just key, Just value ] -> Just $ Tuple key value
        _ -> Nothing

vanityContent :: String -> VanityContent
vanityContent vanity = case lookup vanity vanities of
    Just steamId -> { response: { success: 1, steamid: Just steamId } }
    Nothing -> { response: { success: 42, steamid: Nothing } }

respond :: IncomingMessage -> ServerResponse -> Effect Unit
respond request response =
    case method request, url request <#> split (Pattern "?") of
    Just "GET", Just [ "/ISteamUser/ResolveVanityURL/v1/", query ]
        | Just "steam-is-down" <- lookup "vanityurl" (queryParams query) -> do
            response # writeHead_ 503 (unsafeToForeign {})
            response # end__ # void
        | params <- queryParams query
        , Just vanity <- lookup "vanityurl" params
        , Just key <- lookup "key" params
        , key /= "" -> do
            response # writeHead_ 200 (unsafeToForeign { "content-type": "application/json" })
            response # endString__ (writeJSON $ vanityContent vanity) # void
        | otherwise -> do
            response # writeHead_ 403 (unsafeToForeign {})
            response # end__ # void
    _, _ -> do
        response # writeHead_ 404 (unsafeToForeign {})
        response # end__ # void

main :: Effect Unit
main = createServer_C' respond >>= listen_ { port: 3000 } # void
