module TeamTavern.Server.Profile.Infrastructure.ValidateGamertag (Gamertag, toString, validateGamertag) where

import Prelude

import Data.Either (Either(..))
import Data.String (length, trim)

newtype Gamertag = Gamertag String

toString :: Gamertag -> String
toString (Gamertag gamertag) = gamertag

minNameLength :: Int
minNameLength = 1

maxNameLength :: Int
maxNameLength = 16

-- A Gamertag is the universal name for a player's username on Xbox Live. A Gamertag used online
-- must be unique and can be up to twelve characters in length, including numbers, letters, and spaces.
-- https://en.wikipedia.org/wiki/Xbox_Live#Gamertag

-- It's 16 characters actually.

-- New Xbox players or those who want to change their gamertag can claim one of their choice,
-- up to 12 characters. If a gamertag is already taken, a suffix with numbers is attached to
-- differentiate you from other people with the same gamertag.
-- If you’re assigned a suffix, the # symbol and suffix appear everywhere except in friends lists.
-- Suffixes are 4 digits by default, but they depend on how many gamers have chosen that gamertag.
-- For a 12-character gamertag, we’re limited to a 3-digit suffix.
-- 12 + 1 + 3 = 16
-- https://support.xbox.com/en-US/help/account-profile/profile/gamertag-update-faq
isGamertagValid :: String -> Boolean
isGamertagValid gamertag = minNameLength <= length gamertag && length gamertag <= maxNameLength

validateGamertag :: String -> Either (Array String) Gamertag
validateGamertag gamertag | gamertag' <- trim gamertag =
    if isGamertagValid gamertag'
    then Right $ Gamertag gamertag'
    else Left [ "Invalid Gamertag: " <> gamertag' ]
