module TeamTavern.Server.Profile.Infrastructure.ValidateDiscordTag
    (DiscordTag, DiscordTagError, validateDiscordTag) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (Pattern(..), length, split)
import Data.Variant (Variant)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))
import Wrapped.String (Invalid)

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

validateDiscordTag :: forall errors.
    Maybe String -> ValidatedTerrorNeaVar (discordTag :: {} | errors) (Maybe DiscordTag)
validateDiscordTag discordTag =
    validateContact discordTag isDiscordTagValid DiscordTag (Proxy :: _ "discordTag") ("Invalid DiscordTag: " <> _)
