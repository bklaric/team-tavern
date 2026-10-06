module TeamTavern.Client.Shared.Contacts
    (contactError, contactFormat, contactLabel, contactPlaceholder, steamUnavailable) where

import Data.Maybe (Maybe(..))

-- The accounts players add each other by, one per `game_contact` kind. A game
-- names the ones its players use, and the account holds each the player has
-- given (brief 11.5).

contactLabel :: String -> String
contactLabel = case _ of
    "discord" -> "Discord"
    "steam" -> "Steam profile"
    "riot" -> "Riot ID"
    "battle_tag" -> "BattleTag"
    "ea" -> "EA ID"
    "epic" -> "Epic ID"
    "embark" -> "Embark ID"
    "ubisoft" -> "Ubisoft username"
    "marvel_rivals" -> "Marvel Rivals username"
    "psn" -> "PSN ID"
    "gamer_tag" -> "Xbox gamertag"
    "friend_code" -> "Nintendo friend code"
    kind -> kind

contactPlaceholder :: String -> String
contactPlaceholder = case _ of
    "discord" -> "Your Discord username"
    "steam" -> "steamcommunity.com/id/… or SteamID"
    "riot" -> "Name#TAG"
    "battle_tag" -> "Name#1234"
    "ea" -> "Your EA ID"
    "epic" -> "Your Epic ID"
    "embark" -> "Name#1234"
    "ubisoft" -> "Your Ubisoft username"
    "marvel_rivals" -> "Your Marvel Rivals username"
    "psn" -> "Your PSN ID"
    "gamer_tag" -> "Your gamertag"
    "friend_code" -> "SW-1234-5678-9012"
    _ -> ""

-- | What to give for a contact whose form the label leaves open.
contactFormat :: String -> Maybe String
contactFormat = case _ of
    "steam" -> Just "Paste your Steam profile's link, or your 17-digit SteamID."
    _ -> Nothing

-- | Why the server turned a contact away, where it's more than its length.
contactError :: String -> Maybe String
contactError = case _ of
    "steam" -> Just "That isn't a Steam profile. Paste your profile's link, or your SteamID."
    _ -> Nothing

-- | Steam didn't answer for a custom profile address, which only it can turn
-- | into a SteamID.
steamUnavailable :: String
steamUnavailable = "Steam didn't answer. Try again, or paste your SteamID instead."
