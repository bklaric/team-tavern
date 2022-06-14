module TeamTavern.Server.Team.Infrastructure.ValidateDiscordServer where

import Prelude

import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))
import Data.Traversable (traverse)
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlErrors, validateUrlV)

validateDiscordServer :: forall errors.
    Maybe String -> ValidatedVariants (discordServer :: String | errors) (Maybe Url)
validateDiscordServer discordServer
    = discordServer
    # traverse (validateUrlV "discord.gg")
    # Validated.labelMap (Proxy :: _ "discordServer") \(errors :: UrlErrors) ->
        "Error validating Discord server: " <> show errors
