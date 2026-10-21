-- | Stands in for Discord's token and user endpoints in the local stack, so the
-- | suite can sign up and sign in with Discord without a Discord account. The
-- | code is the user itself: the JSON of the record Discord's user endpoint
-- | would return, with the app and the redirect URI the code was given for,
-- | which the spec, playing Discord's authorize page, sends the browser back
-- | with. As Discord does, the stub swaps each code once, only for the app it
-- | was given to and that same redirect URI, answering `invalid_grant`
-- | otherwise, and refuses another client, a secret other than the
-- | DISCORD_CLIENT_SECRET it shares with the server, or another grant. The
-- | access token it gives is the URI-encoded JSON of the user, which its user
-- | endpoint reads back.
module TeamTavern.DiscordStub.Main (main) where

import Prelude

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, stripPrefix)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import JSURI (decodeURIComponent, encodeURIComponent)
import JavaScript.Node.Buffer (concat_, toString___)
import JavaScript.Node.Http.IncomingMessage (IncomingMessage, header, method, url)
import JavaScript.Node.Http.Server (createServer_C')
import JavaScript.Node.Http.ServerResponse (ServerResponse, writeHead_)
import JavaScript.Node.Net.Server (listen_)
import JavaScript.Node.Process (lookupEnv)
import JavaScript.Node.Stream.Readable.Events (collectDataEvents)
import JavaScript.Node.Stream.Writable (end__, endString__)
import JavaScript.Web.URL.URLSearchParams as URLSearchParams
import TeamTavern.Server.Infrastructure.DiscordSignIn (DiscordUserContent)
import TeamTavern.Shared.Discord (discordClientId)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (readJSON_, writeJSON)

-- | The user a code stands for, and the app and redirect URI it was given for.
type Grant =
    { id :: String
    , username :: String
    , discriminator :: String
    , email :: Maybe String
    , verified :: Maybe Boolean
    , client_id :: String
    , redirect_uri :: String
    }

reply :: Int -> String -> ServerResponse -> Effect Unit
reply status body response = do
    response # writeHead_ status (unsafeToForeign { "content-type": "application/json" })
    response # endString__ body # void

-- Discord's answers to a token request it won't swap.
refuse :: String -> ServerResponse -> Effect Unit
refuse "invalid_client" = reply 401 $ writeJSON { error: "invalid_client" }
refuse error = reply 400 $ writeJSON { error }

accessToken :: Grant -> String
accessToken { id, username, discriminator, email, verified } =
    writeJSON ({ id, username, discriminator, email, verified } :: DiscordUserContent)
    # encodeURIComponent # fromMaybe ""

swap :: String -> Ref (Array String) -> String -> ServerResponse -> Effect Unit
swap secret swapped body response = do
    params <- URLSearchParams.new body
    let param name = URLSearchParams.get name params
    grantType <- param "grant_type"
    clientId <- param "client_id"
    clientSecret <- param "client_secret"
    code <- param "code"
    redirectUri <- param "redirect_uri"
    spent <- Ref.read swapped
    case code, code >>= readJSON_ of
        _, _ | grantType /= Just "authorization_code" -> refuse "unsupported_grant_type" response
        _, _ | clientId /= Just discordClientId || clientSecret /= Just secret -> refuse "invalid_client" response
        Just code', Just (grant :: Grant)
            | not $ elem code' spent
            , clientId == Just grant.client_id
            , redirectUri == Just grant.redirect_uri -> do
                Ref.modify_ (_ <> [ code' ]) swapped
                response # reply 200 (writeJSON
                    { access_token: accessToken grant
                    , token_type: "Bearer"
                    , expires_in: 604800
                    , refresh_token: "stub"
                    , scope: "identify email"
                    })
        _, _ -> refuse "invalid_grant" response

readUser :: IncomingMessage -> Maybe DiscordUserContent
readUser request =
    header "authorization" request
    >>= stripPrefix (Pattern "Bearer ")
    >>= decodeURIComponent
    >>= readJSON_

respond :: String -> Ref (Array String) -> IncomingMessage -> ServerResponse -> Effect Unit
respond secret swapped request response =
    case method request, url request <#> split (Pattern "?"), readUser request of
    Just "POST", Just [ "/api/oauth2/token" ], _ ->
        request # collectDataEvents (map unsafeCoerce >>> concat_ >=> toString___ >=> \body ->
            swap secret swapped body response)
        # void
    Just "GET", Just [ "/api/users/@me" ], Just user ->
        response # reply 200 (writeJSON user)
    Just "GET", Just [ "/api/users/@me" ], Nothing -> do
        response # writeHead_ 401 (unsafeToForeign {})
        response # end__ # void
    _, _, _ -> do
        response # writeHead_ 404 (unsafeToForeign {})
        response # end__ # void

main :: Effect Unit
main = do
    secret <- lookupEnv "DISCORD_CLIENT_SECRET" <#> fromMaybe ""
    swapped <- Ref.new []
    createServer_C' (respond secret swapped) >>= listen_ { port: 3000 } # void
