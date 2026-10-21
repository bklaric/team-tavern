module TeamTavern.Client.Script.Discord
    ( authorizeSwitchToDiscord
    , authorizeWithDiscord
    , keepSwitchCode
    , takeDiscordReturn
    , takeSwitchCode
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import JSURI (decodeURIComponent, encodeURIComponent)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.Trip (Trip, keepSwitch, setOut, takeReturn, takeSwitch)
import TeamTavern.Shared.Discord (discordClientId)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- Every trip to Discord comes back to the sign-in page, the one redirect URI
-- registered on the Discord app for each origin.
storageKey :: String
storageKey = "tt-discord"

switchKey :: String
switchKey = "tt-discord-switch"

-- | Sends the browser to Discord, which sends it to the sign-in page with a
-- | code in the query; the sign-in page then goes on to `back`.
authorizeWithDiscord :: ∀ effect. MonadEffect effect => String -> effect Unit
authorizeWithDiscord back = authorize { back, switching: false }

-- | Sends the browser to Discord to sign the account in with it in place of
-- | its password or Steam, and back to the account page, which does the switch.
authorizeSwitchToDiscord :: ∀ effect. MonadEffect effect => effect Unit
authorizeSwitchToDiscord = authorize { back: "/account", switching: true }

authorize :: ∀ effect. MonadEffect effect => { back :: String, switching :: Boolean } -> effect Unit
authorize trip = liftEffect do
    state <- setOut storageKey trip
    origin' <- window >>= location >>= origin
    hardNavigate $ "https://discord.com/api/oauth2/authorize"
        <> "?client_id=" <> discordClientId
        <> "&redirect_uri=" <> (encodeURIComponent (origin' <> "/signin") # fromMaybe "")
        <> "&response_type=code"
        <> "&scope=identify%20email"
        <> "&state=" <> state
        <> "&prompt=none"

-- | What Discord came back with and the trip it came back from. `code` is for
-- | the server to swap, or nothing when the player turned back at Discord,
-- | which sends `error` with the state instead. Google sends a code and an
-- | error alike, so either is Discord's only with the state of the trip to
-- | Discord this tab kept.
takeDiscordReturn :: ∀ effect. MonadEffect effect => effect (Maybe { code :: Maybe String, trip :: Trip })
takeDiscordReturn = do
    let param name = getQueryParam name <#> (_ >>= decodeURIComponent)
    code <- param "code"
    error <- param "error"
    returnedState <- param "state"
    liftEffect case code, error of
        Nothing, Nothing -> pure Nothing
        _, _ -> takeReturn storageKey { ownMark: false, returnedState } <#> map { code, trip: _ }

-- | Keeps the code of a trip that switches the account to Discord for the
-- | account page.
keepSwitchCode :: ∀ effect. MonadEffect effect => String -> effect Unit
keepSwitchCode = liftEffect <<< keepSwitch switchKey

-- | The code a switch to Discord came back with, taken once.
takeSwitchCode :: ∀ effect. MonadEffect effect => effect (Maybe String)
takeSwitchCode = liftEffect $ takeSwitch switchKey
