-- | Stands in for Steam in the test stack, so the suite can give a custom Steam
-- | profile address and sign in with Steam without reaching Steam.
-- |
-- | Of the Steam Web API it answers ResolveVanityURL and GetPlayerSummaries.
-- | ResolveVanityURL knows one custom address: `gabelogannewell`, Gabe
-- | Newell's. Asked for `steam-is-down` it answers 503, as Steam does when
-- | it's down. Every other name has no profile, and a request without a key is
-- | refused, as Steam refuses it. GetPlayerSummaries names every account
-- | `s ` and the last eight digits of its ID, which the space makes something
-- | a nickname can't be as it is.
-- |
-- | Of Steam's OpenID provider it answers the check of an answer, which it
-- | passes once for each `openid.response_nonce`, as Steam does. The answers
-- | themselves come from the spec, which plays Steam's sign-in page.
module TeamTavern.SteamStub.Main (main) where

import Prelude

import Data.Array (elem, mapMaybe)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, split)
import Data.Tuple (Tuple(..))
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
import TeamTavern.Server.Infrastructure.FetchSteamNickname (SummariesContent)
import TeamTavern.Server.Infrastructure.ResolveSteamId (VanityContent)
import Unsafe.Coerce (unsafeCoerce)
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

summariesContent :: String -> SummariesContent
summariesContent steamId = { response: { players: [ { steamid: steamId, personaname: "s " <> drop 9 steamId } ] } }

reply :: Int -> String -> String -> ServerResponse -> Effect Unit
reply status contentType body response = do
    response # writeHead_ status (unsafeToForeign { "content-type": contentType })
    response # endString__ body # void

refuse :: Int -> ServerResponse -> Effect Unit
refuse status response = do
    response # writeHead_ status (unsafeToForeign {})
    response # end__ # void

-- Steam answers in OpenID's key-value form.
checkAuthentication :: Ref (Array String) -> String -> ServerResponse -> Effect Unit
checkAuthentication checked body response = do
    let params = queryParams body
    valid <- case lookup "openid.mode" params, lookup "openid.response_nonce" params of
        Just "check_authentication", Just nonce -> do
            seen <- Ref.read checked <#> elem nonce
            Ref.modify_ (_ <> [ nonce ]) checked
            pure $ not seen
        _, _ -> pure false
    response # reply 200 "text/plain"
        ("ns:http://specs.openid.net/auth/2.0\nis_valid:" <> (if valid then "true" else "false") <> "\n")

hasKey :: Array (Tuple String String) -> Boolean
hasKey params = case lookup "key" params of
    Just key -> key /= ""
    Nothing -> false

respond :: Ref (Array String) -> IncomingMessage -> ServerResponse -> Effect Unit
respond checked request response =
    case method request, url request <#> split (Pattern "?") of
    Just "GET", Just [ "/ISteamUser/ResolveVanityURL/v1/", query ]
        | Just "steam-is-down" <- lookup "vanityurl" (queryParams query) -> refuse 503 response
        | params <- queryParams query
        , Just vanity <- lookup "vanityurl" params
        , hasKey params -> response # reply 200 "application/json" (writeJSON $ vanityContent vanity)
        | otherwise -> refuse 403 response
    Just "GET", Just [ "/ISteamUser/GetPlayerSummaries/v2/", query ]
        | params <- queryParams query
        , Just steamId <- lookup "steamids" params
        , hasKey params -> response # reply 200 "application/json" (writeJSON $ summariesContent steamId)
        | otherwise -> refuse 403 response
    Just "POST", Just [ "/openid/login" ] ->
        request # collectDataEvents (map unsafeCoerce >>> concat_ >=> toString___ >=> \body ->
            checkAuthentication checked body response)
        # void
    _, _ -> refuse 404 response

main :: Effect Unit
main = do
    checked <- Ref.new []
    createServer_C' (respond checked) >>= listen_ { port: 3000 } # void
