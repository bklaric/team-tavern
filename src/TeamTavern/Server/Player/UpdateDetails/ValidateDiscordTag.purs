module TeamTavern.Server.Player.UpdatePlayer.ValidateDiscordTag
    (DiscordTag, DiscordTagError, validateDiscordTag, validateOptionalDiscordTag) where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split, trim)
import Data.Validated (Validated)
import Data.Validated as Validated
import Data.Variant (Variant)
import Wrapped.String (Invalid, invalid)
import Wrapped.Validated as Wrapped

newtype DiscordTag = DiscordTag String

derive newtype instance showDiscordTag :: Show DiscordTag

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

validateDiscordTag ::
    String -> Validated (NonEmptyList DiscordTagError) DiscordTag
validateDiscordTag nickname =
    Wrapped.create trim [invalid isDiscordTagValid]
    DiscordTag nickname

validateOptionalDiscordTag ::
    Maybe String -> Validated (NonEmptyList DiscordTagError) (Maybe DiscordTag)
validateOptionalDiscordTag discordTag =
    case discordTag of
    Nothing -> Validated.valid Nothing
    Just discordTag' -> validateDiscordTag discordTag' <#> Just
