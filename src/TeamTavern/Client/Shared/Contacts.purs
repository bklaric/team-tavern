module TeamTavern.Client.Shared.Contacts (contactLabel, contactPlaceholder) where

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
    "ubisoft" -> "Ubisoft username"
    "psn" -> "PSN ID"
    "gamer_tag" -> "Xbox gamertag"
    "friend_code" -> "Nintendo friend code"
    kind -> kind

contactPlaceholder :: String -> String
contactPlaceholder = case _ of
    "discord" -> "Your Discord username"
    "steam" -> "steamcommunity.com/id/…"
    "riot" -> "Name#TAG"
    "battle_tag" -> "Name#1234"
    "ea" -> "Your EA ID"
    "ubisoft" -> "Your Ubisoft username"
    "psn" -> "Your PSN ID"
    "gamer_tag" -> "Your gamertag"
    "friend_code" -> "SW-1234-5678-9012"
    _ -> ""
