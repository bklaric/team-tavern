module TeamTavern.Client.Script.Google
    ( authorizeSwitchToGoogle
    , authorizeWithGoogle
    , keepSwitchCode
    , takeGoogleReturn
    , takeSwitchCode
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Class (class MonadEffect, liftEffect)
import JSURI (decodeURIComponent, encodeURIComponent)
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.Trip (Trip, keepSwitch, setOut, takeReturn, takeSwitch)
import TeamTavern.Shared.Google (googleClientId)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- Every trip to Google comes back to the sign-in page, as every trip to
-- Discord does, the redirect URI registered on the OAuth client for each
-- origin.
storageKey :: String
storageKey = "tt-google"

switchKey :: String
switchKey = "tt-google-switch"

-- | Sends the browser to Google, which sends it to the sign-in page with a
-- | code in the query; the sign-in page then goes on to `back`.
authorizeWithGoogle :: ∀ effect. MonadEffect effect => String -> effect Unit
authorizeWithGoogle back = authorize { back, switching: false }

-- | Sends the browser to Google to sign the account in with it in place of
-- | its password, Discord or Steam, and back to the account page, which does
-- | the switch.
authorizeSwitchToGoogle :: ∀ effect. MonadEffect effect => effect Unit
authorizeSwitchToGoogle = authorize { back: "/account", switching: true }

-- `select_account` lets a player signed in to several Google accounts pick.
authorize :: ∀ effect. MonadEffect effect => { back :: String, switching :: Boolean } -> effect Unit
authorize trip = liftEffect do
    state <- setOut storageKey trip
    origin' <- window >>= location >>= origin
    hardNavigate $ "https://accounts.google.com/o/oauth2/v2/auth"
        <> "?client_id=" <> googleClientId
        <> "&redirect_uri=" <> (encodeURIComponent (origin' <> "/signin") # fromMaybe "")
        <> "&response_type=code"
        <> "&scope=openid%20email%20profile"
        <> "&state=" <> state
        <> "&prompt=select_account"

-- | What Google came back with and the trip it came back from. `code` is for
-- | the server to swap, or nothing when the player turned back at Google,
-- | which sends `error` with the state instead. Any provider sends an error,
-- | so one is Google's only with the state of the trip to Google this tab
-- | kept.
takeGoogleReturn :: ∀ effect. MonadEffect effect => effect (Maybe { code :: Maybe String, trip :: Trip })
takeGoogleReturn = do
    let param name = getQueryParam name <#> (_ >>= decodeURIComponent)
    code <- param "code"
    error <- param "error"
    returnedState <- param "state"
    liftEffect case code, error of
        Nothing, Nothing -> pure Nothing
        _, _ -> takeReturn storageKey { ownMark: isJust code, returnedState } <#> map { code, trip: _ }

-- | Keeps the code of a trip that switches the account to Google for the
-- | account page.
keepSwitchCode :: ∀ effect. MonadEffect effect => String -> effect Unit
keepSwitchCode = liftEffect <<< keepSwitch switchKey

-- | The code a switch to Google came back with, taken once.
takeSwitchCode :: ∀ effect. MonadEffect effect => effect (Maybe String)
takeSwitchCode = liftEffect $ takeSwitch switchKey
