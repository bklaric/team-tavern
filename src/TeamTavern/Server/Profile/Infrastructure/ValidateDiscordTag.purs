module TeamTavern.Server.Profile.Infrastructure.ValidateDiscordTag
    (DiscordTag, DiscordTagError, validateDiscordTag) where

import Prelude

import Data.Array (all)
import Data.CodePoint.Unicode (isAlphaNum, isDecDigit, isLower)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), codePointFromChar, length, split, toCodePointArray)
import Data.Variant (Variant)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))
import Wrapped.String (Invalid)

-- https://support.discord.com/hc/en-us/articles/12620128861463
-- Usernames must be at least 2 characters and at most 32 characters long
-- Usernames are case insensitive and forced lowercase
-- Usernames cannot use any other special characters besides underscore ( _ ) and period ( . )
-- Usernames cannot use 2 consecutive period characters ( . )

newtype DiscordTag = DiscordTag String

derive newtype instance Show DiscordTag

type DiscordTagError = Variant (invalid :: Invalid)

minUsernameLength :: Int
minUsernameLength = 2

maxUsernameLength :: Int
maxUsernameLength = 32

discriminatorLength :: Int
discriminatorLength = 4

isDiscordTagValid :: String -> Boolean
isDiscordTagValid discordTag =
    case split (Pattern "#") discordTag of
    [ username, discriminator ] ->
        minUsernameLength <= length username
        && length username <= maxUsernameLength
        && length discriminator == discriminatorLength
    _ -> false

isDiscordUsernameValid :: String -> Boolean
isDiscordUsernameValid username =
    notTooShort && notTooLong && onlyValidChars
    where
    notTooShort = minUsernameLength <= length username
    notTooLong = length username <= maxUsernameLength
    onlyValidChars = username # toCodePointArray # all \char ->
        isLower char || isDecDigit char || char == codePointFromChar '.' || char == codePointFromChar '_'

validateDiscordTag :: ∀ errors.
    Maybe String -> ValidatedTerrorNeaVar (discordTag :: {} | errors) (Maybe DiscordTag)
validateDiscordTag discordTag =
    validateContact discordTag isDiscordContactValid DiscordTag (Proxy :: _ "discordTag") ("Invalid DiscordTag: " <> _)
    where
    isDiscordContactValid contact = isDiscordTagValid contact || isDiscordUsernameValid contact
