module TeamTavern.Server.Team.Infrastructure.ValidateDiscordServer where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as Validated
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlErrors, validateUrlV)

validateDiscordServer :: forall errors.
    Maybe String -> VariantValidated (discordServer :: Array String | errors) (Maybe Url)
validateDiscordServer discordServer
    = discordServer
    # traverse (validateUrlV "discord.gg")
    # Validated.labelMap (SProxy :: SProxy "discordServer") \(errors :: UrlErrors) ->
        [ "Error validating Discord server: " <> show errors]
