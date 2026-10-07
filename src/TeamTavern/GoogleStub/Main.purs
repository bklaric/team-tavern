-- | Stands in for Google's token endpoint in the test stack, so the suite can
-- | sign up and sign in with Google without a Google account. The code is the
-- | account itself: the JSON of the claims the ID token would carry and the
-- | redirect URI the code was given for, which the spec, playing Google's
-- | sign-in page, sends the browser back with. As Google does, the stub swaps
-- | each code once and only for that same redirect URI, answering
-- | `invalid_grant` otherwise, and refuses another client, a secret other than
-- | the GOOGLE_CLIENT_SECRET it shares with the server, or another grant.
module TeamTavern.GoogleStub.Main (main) where

import Prelude

import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import JavaScript.Node.Buffer (concat_, fromString, toString__, toString___)
import JavaScript.Node.Http.IncomingMessage (IncomingMessage, method, url)
import JavaScript.Node.Http.Server (createServer_C')
import JavaScript.Node.Http.ServerResponse (ServerResponse, writeHead_)
import JavaScript.Node.Net.Server (listen_)
import JavaScript.Node.Process (lookupEnv)
import JavaScript.Node.Stream.Readable.Events (collectDataEvents)
import JavaScript.Node.Stream.Writable (end__, endString__)
import JavaScript.Web.URL.URLSearchParams as URLSearchParams
import Literals (StringLit, stringLit)
import TeamTavern.Shared.Google (googleClientId)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (asOneOf)
import Yoga.JSON (readJSON_, writeJSON)

-- | The claims a code stands for, and the redirect URI it was given for.
type Account =
    { sub :: String
    , email :: Maybe String
    , email_verified :: Maybe Boolean
    , name :: Maybe String
    , redirect_uri :: String
    }

base64Url :: String -> Effect String
base64Url string = fromString string (asOneOf (stringLit :: StringLit "utf8"))
    >>= toString__ (asOneOf (stringLit :: StringLit "base64url"))

-- Nothing checks the signature, so the token carries none.
idToken :: Account -> Effect String
idToken { sub, email, email_verified, name } = do
    header <- base64Url $ writeJSON { alg: "none", typ: "JWT" }
    payload <- base64Url $ writeJSON
        { iss: "https://accounts.google.com", aud: googleClientId, sub, email, email_verified, name }
    pure $ header <> "." <> payload <> ".stub"

reply :: Int -> String -> ServerResponse -> Effect Unit
reply status body response = do
    response # writeHead_ status (unsafeToForeign { "content-type": "application/json" })
    response # endString__ body # void

-- Google's answers to a token request it won't swap.
refuse :: String -> ServerResponse -> Effect Unit
refuse "invalid_client" = reply 401 $ writeJSON { error: "invalid_client" }
refuse error = reply 400 $ writeJSON { error }

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
        _, _ | clientId /= Just googleClientId || clientSecret /= Just secret -> refuse "invalid_client" response
        Just code', Just (account :: Account)
            | not $ elem code' spent
            , redirectUri == Just account.redirect_uri -> do
                Ref.modify_ (_ <> [ code' ]) swapped
                token <- idToken account
                response # reply 200 (writeJSON { access_token: "stub", token_type: "Bearer", expires_in: 3599, id_token: token })
        _, _ -> refuse "invalid_grant" response

respond :: String -> Ref (Array String) -> IncomingMessage -> ServerResponse -> Effect Unit
respond secret swapped request response =
    case method request, url request <#> split (Pattern "?") of
    Just "POST", Just [ "/token" ] ->
        request # collectDataEvents (map unsafeCoerce >>> concat_ >=> toString___ >=> \body ->
            swap secret swapped body response)
        # void
    _, _ -> do
        response # writeHead_ 404 (unsafeToForeign {})
        response # end__ # void

main :: Effect Unit
main = do
    secret <- lookupEnv "GOOGLE_CLIENT_SECRET" <#> fromMaybe ""
    swapped <- Ref.new []
    createServer_C' (respond secret swapped) >>= listen_ { port: 3000 } # void
