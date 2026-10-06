-- | A Steam contact as a player gives it. The account holds the SteamID64,
-- | which every Steam tracker takes and the profile's address is built from,
-- | so what a player pastes is read for one.
module TeamTavern.Shared.Steam (SteamInput(..), isSteamId, profileUrl, readSteamInput) where

import Prelude

import Data.Array.NonEmpty as Nea
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, trim)
import JSURI (decodeURIComponent)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (ignoreCase, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

data SteamInput
    = SteamId String
    -- The name of a custom profile address, which only Steam can turn into an ID.
    | Vanity String
    | Unreadable

-- The IDs of individual accounts, the only ones a player can be.
steamIdRegex :: Regex
steamIdRegex = unsafeRegex "^7656119\\d{10}$" noFlags

profileRegex :: Regex
profileRegex = unsafeRegex
    "^(?:https?://)?(?:www\\.)?steamcommunity\\.com/(profiles|id)/([^/?#\\s]+)(?:[/?#].*)?$"
    ignoreCase

isSteamId :: String -> Boolean
isSteamId = test steamIdRegex

profileUrl :: String -> String
profileUrl steamId = "https://steamcommunity.com/profiles/" <> steamId

-- | Reads a SteamID64 or the address of a Steam profile, with or without its
-- | scheme, `www.` or what follows the profile's own path.
readSteamInput :: String -> SteamInput
readSteamInput text
    | isSteamId (trim text) = SteamId (trim text)
    | otherwise = case match profileRegex (trim text) <#> Nea.toArray of
        Just [ _, Just kind, Just segment ]
            | isSteamId segment -> SteamId segment
            -- A browser's address bar shows a custom address percent-encoded.
            | toLower kind == "id" -> Vanity $ fromMaybe segment $ decodeURIComponent segment
        _ -> Unreadable
