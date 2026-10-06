module TeamTavern.Client.Script.Discord
    ( authorizeSwitchToDiscord
    , authorizeWithDiscord
    , keepSwitchToken
    , takeDiscordReturn
    , takeSwitchToken
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import JSURI (decodeURIComponent, encodeURIComponent)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.QueryParams (getFragmentParam)
import TeamTavern.Client.Script.Trip (Trip, keepSwitch, setOut, takeSwitch, takeTrip)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- Every trip to Discord comes back to the sign-in page, the one redirect URI
-- registered on the Discord app for each origin.
storageKey :: String
storageKey = "tt-discord"

switchKey :: String
switchKey = "tt-discord-switch"

-- | Sends the browser to Discord, which sends it to the sign-in page with an
-- | access token in the fragment; the sign-in page then goes on to `back`.
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
        <> "?client_id=1068667687661740052"
        <> "&redirect_uri=" <> (encodeURIComponent (origin' <> "/signin") # fromMaybe "")
        <> "&response_type=token"
        <> "&scope=identify%20email"
        <> "&state=" <> state
        <> "&prompt=none"

-- | The access token Discord came back with, and the trip it came back from.
takeDiscordReturn :: ∀ effect. MonadEffect effect => effect (Maybe { accessToken :: String, trip :: Trip })
takeDiscordReturn = do
    accessToken <- getFragmentParam "access_token" <#> (_ >>= decodeURIComponent)
    returnedState <- getFragmentParam "state" <#> (_ >>= decodeURIComponent)
    liftEffect case accessToken of
        Nothing -> pure Nothing
        Just accessToken' -> takeTrip storageKey returnedState <#> map { accessToken: accessToken', trip: _ }

-- | Keeps the token of a trip that switches the account to Discord for the
-- | account page.
keepSwitchToken :: ∀ effect. MonadEffect effect => String -> effect Unit
keepSwitchToken = liftEffect <<< keepSwitch switchKey

-- | The token a switch to Discord came back with, taken once.
takeSwitchToken :: ∀ effect. MonadEffect effect => effect (Maybe String)
takeSwitchToken = liftEffect $ takeSwitch switchKey
