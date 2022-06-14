module TeamTavern.Server.Profile.Infrastructure.ValidateGamerTag (GamerTag, toString, validateGamerTag) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (length)
import Data.Validated.Label (ValidatedVariants)
import Type.Proxy (Proxy(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)

newtype GamerTag = GamerTag String

toString :: GamerTag -> String
toString (GamerTag gamerTag) = gamerTag

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
isGamerTagValid :: String -> Boolean
isGamerTagValid gamerTag = minNameLength <= length gamerTag && length gamerTag <= maxNameLength

validateGamerTag :: forall errors. Maybe String -> ValidatedVariants (gamerTag :: String | errors) (Maybe GamerTag)
validateGamerTag gamerTag =
    validateContact gamerTag isGamerTagValid GamerTag (Proxy :: _ "gamerTag") ("Invalid GamerTag: " <> _)
