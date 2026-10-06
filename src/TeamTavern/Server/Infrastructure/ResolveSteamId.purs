module TeamTavern.Server.Infrastructure.ResolveSteamId
    (ResolvedAccount, SteamApi(..), VanityContent, resolveSteamContact) where

import Prelude

import Async (Async, attempt, left)
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import JSURI (encodeURIComponent)
import JavaScript.Error (message, name)
import JavaScript.Web.DOM.AbortSignal (timeout)
import JavaScript.Web.Fetch.Async (fetch, status, text)
import TeamTavern.Routes.Shared.Post (AccountContent)
import TeamTavern.Server.Infrastructure.Log (logStamped)
import TeamTavern.Shared.Steam (SteamInput(..), readSteamInput)
import Yoga.JSON.Async (readJSON)

-- | Where the Steam Web API lives, `https://api.steampowered.com` outside the
-- | test stack, and the key it is called with.
newtype SteamApi = SteamApi { url :: String, key :: String }

-- | What ResolveVanityURL answers: `success` is 1 with the ID, or 42 for a
-- | name no profile has.
type VanityContent = { response :: { success :: Int, steamid :: Maybe String } }

-- | The account with the SteamID64 in place of the Steam profile it gave, and
-- | whether Steam failed to answer for a custom address, which then stays as
-- | given.
type ResolvedAccount = { account :: AccountContent, steamUnavailable :: Boolean }

-- The URL carries the key, so no error line names it.
resolveVanity :: SteamApi -> String -> Async (Array String) (Maybe String)
resolveVanity (SteamApi { url, key }) vanity = do
    let encode = \value -> encodeURIComponent value # fromMaybe value
    let vanityUrl = url <> "/ISteamUser/ResolveVanityURL/v1/?key=" <> encode key <> "&vanityurl=" <> encode vanity
    let failure line = [ "Custom address: " <> vanity, line ]
    signal <- liftEffect $ timeout 5000
    response <- fetch vanityUrl { method: "GET", signal } # attempt >>= case _ of
        Left error -> left $ failure $ name error <> " " <> message error
        Right response -> pure response
    body <- text response # lmap (message >>> failure)
    when (status response /= 200) $
        left $ failure $ "Got status " <> show (status response) <> " with content: " <> body
    content :: VanityContent <- readJSON body # lmap (show >>> failure)
    case content.response of
        { success: 1, steamid: Just steamId } -> pure $ Just steamId
        { success: 42 } -> pure Nothing
        _ -> left $ failure $ "Got unexpected content: " <> body

-- | Puts the SteamID64 in place of the Steam profile the account gives, asking
-- | Steam for the ID behind a custom address. What it can't read, or a custom
-- | address no profile has, it leaves for the account's checks to turn away.
-- | A request answered by the checks is never logged, so a failure to reach
-- | Steam is logged here.
resolveSteamContact :: ∀ left. SteamApi -> AccountContent -> Async left ResolvedAccount
resolveSteamContact steamApi account =
    case Object.lookup "steam" account.contacts <#> readSteamInput of
        Just (SteamId steamId) -> pure { account: withSteamId steamId, steamUnavailable: false }
        Just (Vanity vanity) -> resolveVanity steamApi vanity # attempt >>= case _ of
            Right steamId -> pure { account: maybe account withSteamId steamId, steamUnavailable: false }
            Left lines -> do
                liftEffect $ logStamped $ joinWith " | " $ cons "Error resolving Steam profile address" lines
                pure { account, steamUnavailable: true }
        _ -> pure { account, steamUnavailable: false }
    where
    withSteamId steamId = account { contacts = Object.insert "steam" steamId account.contacts }
