module TeamTavern.Server.Infrastructure.FetchDiscordUser
    (DiscordApiUrl(..), DiscordUserContent, fetchDiscordUser, verifiedEmail) where

import Prelude

import Async (Async, attempt, left)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Validated as Validated
import Foreign.Object as Object
import Jarilo (internal__)
import JavaScript.Error (message, name)
import JavaScript.Web.Fetch.Async (fetch, text)
import JavaScript.Web.Fetch.Response (status)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.ValidateEmail (Email, toString, validateEmail)
import Yoga.JSON.Async (readJSON)

-- | Where Discord's API lives, `https://discord.com/api` outside the test stack.
newtype DiscordApiUrl = DiscordApiUrl String

-- | Discord leaves out `email` and `verified` without the `email` scope, and
-- | sends a null email for an account that has none.
type DiscordUserContent =
    { id :: String
    , username :: String
    , discriminator :: String
    , email :: Maybe String
    , verified :: Maybe Boolean
    }

-- | Discord vouches for owning the address, not for its shape, so it passes the
-- | same validation as an address a player types in.
verifiedEmail :: DiscordUserContent -> Maybe String
verifiedEmail { email: Just email, verified: Just true } =
    (validateEmail email :: ValidatedTerrorNeaVar (email :: {}) Email)
    # Validated.hush
    <#> toString
verifiedEmail _ = Nothing

fetchDiscordUser :: forall responses.
    DiscordApiUrl -> String -> Async (InternalTerror_ responses) DiscordUserContent
fetchDiscordUser (DiscordApiUrl apiUrl) accessToken = do
    let userUrl = apiUrl <> "/users/@me"
    let options =
            { method: "GET"
            , headers: Object.singleton "Authorization" ("Bearer " <> accessToken)
            }
    result <- fetch userUrl options # attempt
    case result of
        Left error -> left $ Terror internal__
            ["Error fetching user data from Discord: "
                <> name error <> " " <> message error]
        Right response ->
            if status response == 200
            then do
                (text response # lmap \error' -> Terror internal__
                    ["Error reading Discord user content: " <> message error'])
                >>= (readJSON >>> lmap \error' -> Terror internal__
                    ["Error parsing Discord user content: " <> show error'])
            else do
                text' <- text response # lmap \error' -> Terror internal__
                    ["Error reading Discord user content: " <> message error']
                left $ Terror internal__
                    [ "Got unexpected response from Discord user endpoint: "
                        <> (show $ status response)
                    , "With following content: " <> text'
                    ]
