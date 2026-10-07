-- | A trip to Discord, Steam or Google, which comes back to the sign-in page.
-- | What the player was doing rides along in session storage, under a random
-- | state the provider hands back, so what arrives from a trip this tab didn't
-- | start is refused.
module TeamTavern.Client.Script.Trip (Trip, keepSwitch, setOut, takeReturn, takeSwitch) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import TeamTavern.Client.Script.Navigate (replaceState)
import Web.HTML (window)
import Web.HTML.Location (pathname, search)
import Web.HTML.Window (location, sessionStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Yoga.JSON (class ReadForeign, class WriteForeign, readJSON_, writeJSON)

-- 128 random bits as 32 hex digits.
foreign import randomState :: Effect String

-- | `back` is where the trip goes on to, and `from` the page it set out from,
-- | which a player who turns back returns to. A trip that switches the
-- | account's sign-in signs nobody in: the sign-in page hands what came back on
-- | to the account page.
type Trip = { back :: String, from :: String, switching :: Boolean }

type Kept = { state :: String, back :: String, from :: String, switching :: Boolean }

-- | Keeps a trip setting out from this page under `key`, and gives the state
-- | for the provider to hand back.
setOut :: String -> { back :: String, switching :: Boolean } -> Effect String
setOut key { back, switching } = do
    state <- randomState
    location' <- window >>= location
    from <- (<>) <$> pathname location' <*> search location'
    window >>= sessionStorage >>= setItem key (writeJSON ({ state, back, from, switching } :: Kept))
    pure state

-- | The trip kept under `key`, taken once, if what came back is its own.
-- | `ownMark` is whether it carries what only this provider sends, which
-- | claims it, and the trip is then given if `returnedState` is its own. An
-- | error every provider sends alike is claimed only with the kept trip's
-- | state. A claimed return leaves the page's address without its query and
-- | fragment, so a reload doesn't send it twice.
takeReturn :: String -> { ownMark :: Boolean, returnedState :: Maybe String } -> Effect (Maybe Trip)
takeReturn key { ownMark, returnedState } = do
    storage <- window >>= sessionStorage
    kept <- getItem key storage <#> (_ >>= readJSON_)
    let keptState = kept <#> \({ state } :: Kept) -> state
    if not ownMark && (returnedState == Nothing || keptState /= returnedState)
    then pure Nothing
    else do
        removeItem key storage
        window >>= location >>= pathname >>= replaceState {}
        pure case kept of
            Just ({ state, back, from, switching } :: Kept) | Just state == returnedState ->
                Just { back, from, switching }
            _ -> Nothing

-- | Keeps what a switching trip came back with, under `key`, for the account
-- | page.
keepSwitch :: ∀ value. WriteForeign value => String -> value -> Effect Unit
keepSwitch key value = window >>= sessionStorage >>= setItem key (writeJSON value)

-- | What a switching trip came back with, taken once.
takeSwitch :: ∀ value. ReadForeign value => String -> Effect (Maybe value)
takeSwitch key = do
    storage <- window >>= sessionStorage
    value <- getItem key storage <#> (_ >>= readJSON_)
    removeItem key storage
    pure value
