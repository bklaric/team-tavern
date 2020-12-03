module TeamTavern.Server.Player.UpdatePlayer.ValidateDiscordTag
    (DiscordTag, DiscordTagError, validateDiscordTag) where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split, trim)
import Data.Validated as Validated
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as ValidatedLabel
import Data.Variant (SProxy(..), Variant)
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

validateDiscordTag :: forall errors.
    Maybe String -> VariantValidated (discordTag :: Array String | errors) (Maybe DiscordTag)
validateDiscordTag discordTag =
    case discordTag of
    Nothing -> Validated.valid Nothing
    Just discordTag' ->
        Wrapped.create trim [invalid isDiscordTagValid] DiscordTag discordTag'
        <#> Just
        # ValidatedLabel.labelMap (SProxy :: SProxy "discordTag")
            \(errors :: NonEmptyList DiscordTagError) ->
                [ "Error validating Discord tag: " <> show errors ]
