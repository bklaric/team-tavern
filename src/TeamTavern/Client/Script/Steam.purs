module TeamTavern.Client.Script.Steam
    ( authorizeSwitchToSteam
    , authorizeWithSteam
    , keepSwitchAssertion
    , takeSteamReturn
    , takeSwitchAssertion
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import JSURI (decodeURIComponent)
import JavaScript.Web.URL.URLSearchParams as URLSearchParams
import TeamTavern.Client.Script.Navigate (hardNavigate)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Script.Trip (Trip, keepSwitch, setOut, takeReturn, takeSwitch)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

-- The `openid.` parameters of the page's address.
foreign import openIdParams :: Effect (Object String)

-- Every trip to Steam comes back to the sign-in page, as every trip to Discord
-- does, with the trip's state in the address it returns to.
storageKey :: String
storageKey = "tt-steam"

switchKey :: String
switchKey = "tt-steam-switch"

-- What every OpenID sign-in names in place of the account, which Steam then
-- asks for.
identifierSelect :: String
identifierSelect = "http://specs.openid.net/auth/2.0/identifier_select"

-- | Sends the browser to Steam, which sends it to the sign-in page with its
-- | answer in the query; the sign-in page then goes on to `back`.
authorizeWithSteam :: ∀ effect. MonadEffect effect => String -> effect Unit
authorizeWithSteam back = authorize { back, switching: false }

-- | Sends the browser to Steam to sign the account in with it in place of its
-- | password or Discord, and back to the account page, which does the switch.
authorizeSwitchToSteam :: ∀ effect. MonadEffect effect => effect Unit
authorizeSwitchToSteam = authorize { back: "/account", switching: true }

authorize :: ∀ effect. MonadEffect effect => { back :: String, switching :: Boolean } -> effect Unit
authorize trip = liftEffect do
    state <- setOut storageKey trip
    origin' <- window >>= location >>= origin
    query <- URLSearchParams.new (Object.fromHomogeneous
        { "openid.ns": "http://specs.openid.net/auth/2.0"
        , "openid.mode": "checkid_setup"
        , "openid.return_to": origin' <> "/signin?steam=" <> state
        , "openid.realm": origin'
        , "openid.identity": identifierSelect
        , "openid.claimed_id": identifierSelect
        })
        >>= URLSearchParams.toString
    hardNavigate $ "https://steamcommunity.com/openid/login?" <> query

-- | What Steam came back with and the trip it came back from. `assertion` is
-- | Steam's answer, for the server to check, or nothing when the player turned
-- | back at Steam.
takeSteamReturn :: ∀ effect. MonadEffect effect =>
    effect (Maybe { assertion :: Maybe (Object String), trip :: Trip })
takeSteamReturn = do
    returnedState <- getQueryParam "steam" <#> (_ >>= decodeURIComponent)
    liftEffect case returnedState of
        Nothing -> pure Nothing
        Just _ -> do
            params <- openIdParams
            let assertion = if Object.lookup "openid.mode" params == Just "id_res" then Just params else Nothing
            takeReturn storageKey { ownMark: true, returnedState } <#> map { assertion, trip: _ }

-- | Keeps the answer of a trip that switches the account to Steam for the
-- | account page.
keepSwitchAssertion :: ∀ effect. MonadEffect effect => Object String -> effect Unit
keepSwitchAssertion = liftEffect <<< keepSwitch switchKey

-- | The answer a switch to Steam came back with, taken once.
takeSwitchAssertion :: ∀ effect. MonadEffect effect => effect (Maybe (Object String))
takeSwitchAssertion = liftEffect $ takeSwitch switchKey
