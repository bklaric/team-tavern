module TeamTavern.Client.Script.Discord
    ( authorizeSwitchToDiscord
    , authorizeWithDiscord
    , keepSwitchToken
    , takeDiscordReturn
    , takeSwitchToken
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import JSURI (decodeURIComponent, encodeURIComponent)
import TeamTavern.Client.Script.Navigate (hardNavigate, replaceState)
import TeamTavern.Client.Script.QueryParams (getFragmentParam)
import Web.HTML (window)
import Web.HTML.Location (origin, pathname, search)
import Web.HTML.Window (location, sessionStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Yoga.JSON (readJSON_, writeJSON)

foreign import randomState :: Effect String

-- Every trip to Discord comes back to the sign-in page, the one redirect URI
-- registered on the Discord app for each origin. What the player was doing
-- rides along in session storage, under a random state that Discord hands back,
-- so a token arriving from a trip this tab didn't start is refused.
storageKey :: String
storageKey = "tt-discord"

-- A trip that switches the account to Discord signs nobody in: the sign-in
-- page hands its token on to the account page.
type Trip = { state :: String, back :: String, switching :: Boolean }

-- | Sends the browser to Discord, which sends it to the sign-in page with an
-- | access token in the fragment; the sign-in page then goes on to `back`.
authorizeWithDiscord :: ∀ effect. MonadEffect effect => String -> effect Unit
authorizeWithDiscord back = authorize { back, switching: false }

-- | Sends the browser to Discord to sign the account in with it in its
-- | password's place, and back to the account page, which does the switch.
authorizeSwitchToDiscord :: ∀ effect. MonadEffect effect => effect Unit
authorizeSwitchToDiscord = authorize { back: "/account", switching: true }

authorize :: ∀ effect. MonadEffect effect => { back :: String, switching :: Boolean } -> effect Unit
authorize { back, switching } = liftEffect do
    state <- randomState
    window >>= sessionStorage >>= setItem storageKey (writeJSON ({ state, back, switching } :: Trip))
    origin' <- window >>= location >>= origin
    hardNavigate $ "https://discord.com/api/oauth2/authorize"
        <> "?client_id=1068667687661740052"
        <> "&redirect_uri=" <> (encodeURIComponent (origin' <> "/signin") # fromMaybe "")
        <> "&response_type=token"
        <> "&scope=identify%20email"
        <> "&state=" <> state
        <> "&prompt=none"

-- | The access token Discord came back with, where the trip was headed and
-- | whether it switches the account to Discord. The fragment is cleared either
-- | way, so a reload doesn't use the token twice.
takeDiscordReturn :: ∀ effect. MonadEffect effect =>
    effect (Maybe { accessToken :: String, back :: String, switching :: Boolean })
takeDiscordReturn = do
    accessToken <- getFragmentParam "access_token" <#> (_ >>= decodeURIComponent)
    returnedState <- getFragmentParam "state" <#> (_ >>= decodeURIComponent)
    liftEffect case accessToken of
        Nothing -> pure Nothing
        Just accessToken' -> do
            storage <- window >>= sessionStorage
            trip <- getItem storageKey storage <#> (_ >>= readJSON_)
            removeItem storageKey storage
            location' <- window >>= location
            path <- pathname location'
            query <- search location'
            replaceState {} (path <> query)
            pure case trip of
                Just ({ state, back, switching } :: Trip) | Just state == returnedState ->
                    Just { accessToken: accessToken', back, switching }
                _ -> Nothing

switchKey :: String
switchKey = "tt-discord-switch"

-- | Keeps the token of a trip that switches the account to Discord for the
-- | account page.
keepSwitchToken :: ∀ effect. MonadEffect effect => String -> effect Unit
keepSwitchToken accessToken = liftEffect $ window >>= sessionStorage >>= setItem switchKey accessToken

-- | The token a switch to Discord came back with, taken once.
takeSwitchToken :: ∀ effect. MonadEffect effect => effect (Maybe String)
takeSwitchToken = liftEffect do
    storage <- window >>= sessionStorage
    accessToken <- getItem switchKey storage
    removeItem switchKey storage
    pure accessToken
