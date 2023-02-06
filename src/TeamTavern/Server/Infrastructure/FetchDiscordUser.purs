module TeamTavern.Server.Infrastructure.FetchDiscordUser where

import Prelude

import Async (Async, attempt, left)
import Browser.Async.Fetch (fetch, method)
import Browser.Async.Fetch.Response (text)
import Browser.Fetch (headers)
import Browser.Fetch.Response (status)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Options ((:=))
import Error.Class (message, name)
import Jarilo (internal__)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Yoga.JSON.Async (readJSON)

type DiscordUserContent =
    { id :: String
    , username :: String
    , discriminator :: String
    }

fetchDiscordUser :: forall responses.
    String -> Async (InternalTerror_ responses) DiscordUserContent
fetchDiscordUser accessToken = do
    let userUrl = "https://discord.com/api/users/@me"
    let options = method := GET
            <> headers := {"Authorization": "Bearer " <> accessToken}
    result <- fetch userUrl options # attempt
    case result of
        Left error -> left $ Terror internal__
            ["Error fetching user data from Discord: "
                <> name error <> " " <> message error]
        Right response ->
            if status response == 200
            then do
                text response >>= readJSON # lmap \error' -> Terror internal__
                    ["Error parsing Discord user content: " <> show error']
            else do
                text' <- text response
                left $ Terror internal__
                    [ "Got unexpected response from Discord user endpoint: "
                        <> (show $ status response)
                    , "With following content: " <> text'
                    ]
