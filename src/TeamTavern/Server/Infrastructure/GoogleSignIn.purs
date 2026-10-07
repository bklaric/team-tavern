module TeamTavern.Server.Infrastructure.GoogleSignIn
    (GoogleClient(..), GoogleUser, exchangeGoogleCode, googleEndpoint) where

import Prelude

import Async (Async, attempt, left)
import Data.Array (elem)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.Variant (Variant, inj)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Error (message, name)
import JavaScript.Node.Buffer (fromString, toString___)
import JavaScript.Web.DOM.AbortSignal (timeout)
import JavaScript.Web.Fetch.Async (fetch, status, text)
import JavaScript.Web.URL.URLSearchParams as URLSearchParams
import Literals (StringLit, stringLit)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.ValidateEmail (VouchedEmail, vouchedEmail)
import TeamTavern.Server.Player.Domain.Nickname (nicknameOf)
import TeamTavern.Shared.Google (googleClientId)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Untagged.Union (asOneOf)
import Yoga.JSON (readJSON_)
import Yoga.JSON.Async (readJSON)

-- | `tokenUrl` is where Google swaps a code for the tokens of the account it
-- | is for, and `secret` the OAuth client's, which the swap takes.
newtype GoogleClient = GoogleClient { tokenUrl :: String, secret :: String }

-- | Google's token endpoint, which the site asks outside the test stack.
googleEndpoint :: String
googleEndpoint = "https://oauth2.googleapis.com/token"

-- | The claims of Google's ID token the site reads. `email` and `name` come
-- | with the `email` and `profile` scopes.
type IdTokenClaims =
    { iss :: String
    , aud :: String
    , sub :: String
    , email :: Maybe String
    , email_verified :: Maybe Boolean
    , name :: Maybe String
    }

-- | The account Google says signed in: the subject Google names it by, its
-- | address and whether Google verified it, which confirms it, and its name as
-- | a nickname, empty where Google gave none.
type GoogleUser =
    { googleId :: String
    , email :: Maybe VouchedEmail
    , nickname :: String
    }

type ExchangeError other errors =
    TerrorVar (InternalRow_ + BadRequestRow (Variant (googleRefused :: {} | other)) + errors)

-- What Google calls itself in the tokens it gives.
issuers :: Array String
issuers = [ "accounts.google.com", "https://accounts.google.com" ]

internal :: ∀ other errors a. Array String -> Async (ExchangeError other errors) a
internal = left <<< Terror internal__

-- | The account the code Google sent the browser back to this origin's
-- | sign-in page with is for. The ID token comes from Google itself, so its
-- | claims hold without checking its signature. Google swaps a code once, and
-- | only for the page it sent it to, so a reload, or a code another site got,
-- | is refused as `googleRefused`. Google failing to answer is `internal`.
exchangeGoogleCode :: ∀ other errors.
    GoogleClient -> String -> String -> Async (ExchangeError other errors) GoogleUser
exchangeGoogleCode (GoogleClient { tokenUrl, secret }) origin code = do
    body <- liftEffect $ URLSearchParams.new
        ( Object.fromHomogeneous
            { code
            , client_id: googleClientId
            , client_secret: secret
            , redirect_uri: origin <> "/signin"
            , grant_type: "authorization_code"
            }
        )
        >>= URLSearchParams.toString
    signal <- liftEffect $ timeout 5000
    response <- fetch tokenUrl
        { method: "POST"
        , headers: Object.singleton "Content-Type" "application/x-www-form-urlencoded"
        , body
        , signal
        }
        # attempt >>= case _ of
            Left error -> internal [ "Error asking Google to swap a code: " <> name error <> " " <> message error ]
            Right response -> pure response
    content <- text response # lmap \error -> Terror internal__
        [ "Error reading Google's answer to swapping a code: " <> message error ]
    case status response, readJSON_ content :: Maybe { error :: String } of
        200, _ -> pure unit
        400, Just { error: "invalid_grant" } -> left $ Terror (badRequest_ $ inj (Proxy :: _ "googleRefused") {})
            [ "Google refused to swap the code: " <> content ]
        status', _ -> internal [ "Google swapped a code with " <> show status' <> ": " <> content ]
    { id_token } :: { id_token :: String } <- readJSON content # lmap \error -> Terror internal__
        [ "Error parsing Google's tokens: " <> show error ]
    payload <- case split (Pattern ".") id_token of
        [ _, payload, _ ] -> liftEffect $ fromString payload (asOneOf (stringLit :: StringLit "base64url")) >>= toString___
        _ -> internal [ "Google's ID token isn't one: " <> id_token ]
    claims :: IdTokenClaims <- readJSON payload # lmap \error -> Terror internal__
        [ "Error parsing the claims of Google's ID token: " <> show error, payload ]
    unless (claims.aud == googleClientId && elem claims.iss issuers) $
        internal [ "Google's ID token is for another client or from another issuer: " <> payload ]
    pure
        { googleId: claims.sub
        , email: vouchedEmail claims.email claims.email_verified
        , nickname: maybe "" nicknameOf claims.name
        }
