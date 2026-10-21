module TeamTavern.Server.Infrastructure.DiscordSignIn
    (DiscordClient(..), DiscordUser, DiscordUserContent, discordEndpoint, exchangeDiscordCode) where

import Prelude

import Async (Async, attempt, left)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__)
import JavaScript.Error (message, name)
import JavaScript.Web.DOM.AbortSignal (timeout)
import JavaScript.Web.Fetch.Async (fetch, status, text)
import JavaScript.Web.URL.URLSearchParams as URLSearchParams
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.ValidateEmail (VouchedEmail, vouchedEmail)
import TeamTavern.Shared.Discord (discordClientId)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Yoga.JSON (readJSON_)
import Yoga.JSON.Async (readJSON)

-- | `apiUrl` is where Discord's API lives, which swaps a code for a token and
-- | names the user the token is for, and `secret` the app's, which the swap
-- | takes.
newtype DiscordClient = DiscordClient { apiUrl :: String, secret :: String }

-- | Discord's API, which the site asks outside the local stack.
discordEndpoint :: String
discordEndpoint = "https://discord.com/api"

-- | Discord leaves out `email` and `verified` without the `email` scope, and
-- | sends a null email for an account that has none.
type DiscordUserContent =
    { id :: String
    , username :: String
    , discriminator :: String
    , email :: Maybe String
    , verified :: Maybe Boolean
    }

-- | The account Discord says signed in: the id Discord knows it by, its
-- | address and whether Discord verified it, which confirms it, its username
-- | as a nickname, and its tag, what other players type to add it on Discord.
type DiscordUser =
    { discordId :: String
    , email :: Maybe VouchedEmail
    , username :: String
    , tag :: String
    }

type ExchangeError other errors =
    TerrorVar (InternalRow_ + BadRequestRow (Variant (discordRefused :: {} | other)) + errors)

internal :: ∀ other errors a. Array String -> Async (ExchangeError other errors) a
internal = left <<< Terror internal__

-- The username alone since Discord dropped discriminators, which it now
-- reports as "0".
discordTag :: DiscordUserContent -> String
discordTag { username, discriminator: "0" } = username
discordTag { username, discriminator } = username <> "#" <> discriminator

-- | The account the code Discord sent the browser back to this origin's
-- | sign-in page with is for. Discord swaps a code only for the app it gave it
-- | to, once, and only for the page it sent it to, so a code another app got,
-- | or a reload, is refused as `discordRefused`. Discord failing to answer is
-- | `internal`.
exchangeDiscordCode :: ∀ other errors.
    DiscordClient -> String -> String -> Async (ExchangeError other errors) DiscordUser
exchangeDiscordCode (DiscordClient { apiUrl, secret }) origin code = do
    body <- liftEffect $ URLSearchParams.new
        ( Object.fromHomogeneous
            { code
            , client_id: discordClientId
            , client_secret: secret
            , redirect_uri: origin <> "/signin"
            , grant_type: "authorization_code"
            }
        )
        >>= URLSearchParams.toString
    swapSignal <- liftEffect $ timeout 5000
    swapResponse <- fetch (apiUrl <> "/oauth2/token")
        { method: "POST"
        , headers: Object.singleton "Content-Type" "application/x-www-form-urlencoded"
        , body
        , signal: swapSignal
        }
        # attempt >>= case _ of
            Left error -> internal [ "Error asking Discord to swap a code: " <> name error <> " " <> message error ]
            Right response -> pure response
    swapContent <- text swapResponse # lmap \error -> Terror internal__
        [ "Error reading Discord's answer to swapping a code: " <> message error ]
    case status swapResponse, readJSON_ swapContent :: Maybe { error :: String } of
        200, _ -> pure unit
        400, Just { error: "invalid_grant" } -> left $ Terror (badRequest_ $ inj (Proxy :: _ "discordRefused") {})
            [ "Discord refused to swap the code: " <> swapContent ]
        status', _ -> internal [ "Discord swapped a code with " <> show status' <> ": " <> swapContent ]
    { access_token } :: { access_token :: String } <- readJSON swapContent # lmap \error -> Terror internal__
        [ "Error parsing Discord's token: " <> show error ]
    userSignal <- liftEffect $ timeout 5000
    userResponse <- fetch (apiUrl <> "/users/@me")
        { method: "GET"
        , headers: Object.singleton "Authorization" ("Bearer " <> access_token)
        , signal: userSignal
        }
        # attempt >>= case _ of
            Left error -> internal [ "Error asking Discord for the user: " <> name error <> " " <> message error ]
            Right response -> pure response
    userContent <- text userResponse # lmap \error -> Terror internal__
        [ "Error reading Discord's user: " <> message error ]
    unless (status userResponse == 200) $
        internal [ "Discord named the user with " <> show (status userResponse) <> ": " <> userContent ]
    user :: DiscordUserContent <- readJSON userContent # lmap \error -> Terror internal__
        [ "Error parsing Discord's user: " <> show error ]
    pure
        { discordId: user.id
        , email: vouchedEmail user.email user.verified
        , username: user.username
        , tag: discordTag user
        }
